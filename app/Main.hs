{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Main where

import           Api
import           Control.Concurrent         (newEmptyMVar, putMVar, takeMVar,
                                             threadDelay)
import           Control.Concurrent.Async   (Async, cancel)
import           Control.Concurrent.MVar    (MVar)
import           Control.Monad              (guard, void, when)
import           Control.Monad.Trans.Class  (MonadTrans (lift))
import           Control.Monad.Trans.Reader (ask, asks, mapReaderT, runReaderT)
import           Env
import           Interaction
import           Menu
import           Network.HTTP.Client        (defaultManagerSettings, newManager)
import           Prelude                    hiding (log)
import           Servant.Client             (ClientError (..), mkClientEnv,
                                             parseBaseUrl, runClientM, (//))
import           Shelly                     (Sh, echo, errExit, errorExit,
                                             lastStderr, liftIO, run_, shelly,
                                             silently, tracing, verbosely)
import           Shelly.Background          (background, jobs)

main :: IO ()
main = do
    manager <- newManager defaultManagerSettings
    baseUrl <- parseBaseUrl "http://localhost:8087"
    let env = mkClientEnv manager baseUrl

    shelly $ verbosely $ runReaderT app (Env env (Menu dmenu) (lift . echo))

-- doesn't show password in logs
-- signifcantly faster, uses bw serve
-- case insensitivty
-- Force user to choose name first for any item (required)
app :: App ()
app = do
    envC <- asks envClient
    env <- ask
    res <- fmap unVaultResponse <$> liftIO (runClientM (vaultClient // miscEp // statusEp) envC)
    mpw <- liftIO newEmptyMVar
    log <- asks envLog
    case res of
        -- server is not running.
        Left err -> case err of
            -- Most frequently, user is logged in but server is not running (so start it)
            (ConnectionError _) ->
                void $
                    lift
                        ( jobs
                            2
                            ( \job -> do
                                asyncClient <- background job (runReaderT (startClient mpw) env)
                                _asyncServer <- background job (startServer asyncClient mpw env)
                                return ()
                            )
                        )
            _ -> return ()
        -- server is running, just run client
        Right status -> do
            when
                (statusLockStatus status == Locked)
                (mapReaderT silently (log "Unlocking" >> interactionLoop unlockI))

            items <- getItems

            interactionLoop (dashboardI items)

startServer :: Async () -> MVar Password -> Env -> Sh ()
startServer ac mpw env =
    errExit False $ do
        run_ "bw" ["serve"]
        echo "Server failed to start"
        out <- lastStderr
        if out == "\ESC[91mYou are not logged in.\ESC[39m\n"
            then
                runReaderT
                    ( mapReaderT silently $ do
                        -- log in
                        Menu menu <- asks envMenu
                        Left email <- lift $ menu (Prompt [ArgPrompt "Enter Email"]) []
                        lift $ echo email
                        when (email == "") (liftIO (cancel ac) >> lift (errorExit "Empty email"))
                        Left pw <- lift $ menu (Prompt [ArgPrompt "Enter Password", ArgObscured]) []
                        when (pw == "") (liftIO (cancel ac) >> lift (errorExit "Empty password"))
                        lift (tracing False $ run_ "bw" ["login", "--nointeraction", email, pw])
                        liftIO $ putMVar mpw (Password pw)
                        -- try again
                        lift (startServer ac mpw env)
                    )
                    env
            else liftIO (cancel ac)

startClient :: MVar Password -> App ()
startClient mpw = do
    envC <- asks envClient
    log <- asks envLog
    res <- fmap unVaultResponse <$> liftIO (runClientM (vaultClient // miscEp // statusEp) envC)
    case res of
        Left _ -> log "Waiting... " >> liftIO (threadDelay 500) >> startClient mpw
        Right s
            | statusLockStatus s == Unlocked -> getItems >>= interactionLoop . dashboardI
            | otherwise -> do
                pw <- liftIO (takeMVar mpw)
                log "Unlocking using password from login"
                unlock pw
                startClient mpw
