{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Main where

import           Api
import           Control.Concurrent         (newEmptyMVar, putMVar, takeMVar,
                                             threadDelay, tryTakeMVar)
import           Control.Concurrent.Async   (Async, async, cancel)
import           Control.Concurrent.MVar    (MVar)
import           Control.Monad              (void, when)
import           Control.Monad.Trans.Class  (MonadTrans (lift))
import           Control.Monad.Trans.Reader (ask, asks, mapReaderT, runReaderT)
import           Data.Aeson                 (FromJSON, decode, parseJSON,
                                             withObject, (.:))
import           Data.ByteString.Lazy       as BL
import           Data.Coerce                (coerce)
import           Data.Text                  (Text)
import qualified Data.Text.Encoding         as T
import           Env
import           Interaction
import           Menu
import           Network.HTTP.Client        (defaultManagerSettings, newManager)
import           Prelude                    hiding (log)
import           Servant.Client             (mkClientEnv, parseBaseUrl,
                                             runClientM, (//))
import           Shelly                     (Sh, echo, errExit, errorExit,
                                             lastStderr, liftIO, print_stdout,
                                             run, run_, shelly, silently,
                                             tracing, verbosely)
import           Shelly.Background          (background, jobs)

main :: IO ()
main = do
    manager <- newManager defaultManagerSettings
    baseUrl <- parseBaseUrl "http://localhost:8087"
    let env = mkClientEnv manager baseUrl

    shelly $ verbosely $ runReaderT app (Env env (Menu dmenu) (lift . echo))

data AuthStatus = Authenticated | Unauthenticated | Undecided
    deriving (Show, Eq)

data ServerStatus = ServerOffline AuthStatus | ServerOnline LockStatus
    deriving (Show, Eq)

newtype BwStatus = BwStatus
    { bwStatus :: AuthStatus
    }
    deriving (Show, Eq)

instance FromJSON BwStatus where
    parseJSON = withObject "BwStatus" $ \o -> do
        status :: Text <- o .: "status"
        pure . BwStatus $ case status of
            "unauthenticated" -> Unauthenticated
            _                 -> Authenticated

serverStatus :: App ServerStatus
serverStatus = do
    client <- asks envClient
    status <- liftIO (fmap coerce <$> runClientM (vaultClient // miscEp // statusEp) client)
    case status of
        Right s -> pure (ServerOnline (statusLockStatus s))
        Left _ -> do
            statusJson <- lift (print_stdout False $ run "bw" ["status"])
            let mstatus = decode . BL.fromStrict . T.encodeUtf8 $ statusJson
            pure . ServerOffline $ case mstatus of
                Just (BwStatus bs) -> bs
                Nothing            -> Undecided

-- doesn't show password in logs
-- signifcantly faster, uses bw serve
-- case insensitivty
-- Force user to choose name first for any item (required)
app :: App ()
app = do
    status <- serverStatus
    mpw <- liftIO newEmptyMVar
    actOnStatus mpw status

actOnStatus :: MVar Password -> ServerStatus -> App ()
actOnStatus mpw status = do
    env <- ask
    log <- asks envLog
    case status of
        ServerOffline Undecided -> lift (errorExit "Weird status response from bitwarden")
        ServerOffline Unauthenticated -> do
            -- authenticate
            log "ServerOffline, Unauthenticated: logging in"
            lift $
                runReaderT
                    ( mapReaderT silently $ do
                        Menu menu <- asks envMenu
                        Left email <- lift $ menu (Prompt [ArgPrompt "Enter Email"]) []
                        when (email == "") (lift (errorExit "Empty email"))
                        Left pw <- lift $ menu (Prompt [ArgPrompt "Enter Password", ArgObscured]) []
                        when (pw == "") (lift (errorExit "Empty password"))
                        lift (tracing False $ run_ "bw" ["login", "--nointeraction", email, pw])
                        liftIO $ putMVar mpw (Password pw)
                        actOnStatus mpw =<< serverStatus
                    )
                    env
        ServerOffline Authenticated -> do
            log "ServerOffline, Authenticated: 2 threads: starting serve, acting on status"
            lift
                ( jobs
                    2
                    ( \job -> do
                        a :: Async () <- background job (errExit False $ run_ "bw" ["serve"])
                        void (background job (runReaderT (actOnStatus mpw =<< serverStatus) env))
                    )
                )
        ServerOnline Unlocked -> getItems >>= interactionLoop . dashboardI
        ServerOnline Locked -> do
            log "ServerOnline, Locked: trying to unlock"
            maybePw <- liftIO (tryTakeMVar mpw)
            case maybePw of
                Just pw -> void (unlock pw)
                Nothing -> mapReaderT silently $ do
                    envLog env "Unlocking"
                    interactionLoop unlockI

            items <- getItems
            interactionLoop (dashboardI items)

--  ( jobs
--      2
--      ( \job -> do
--          asyncClient <- background job (runReaderT (startClient mpw) env)
--          _asyncServer <- background job (startServer asyncClient mpw env)
--          return ()
--      )
--  )

startServer :: Async () -> MVar Password -> Env -> Sh ()
startServer ac mpw env =
    errExit False $ do
        run_ "bw" ["serve"]
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

bwServe :: App ()
bwServe = do
    async (lift $ errExit False $ run_ "bw" ["serve"])
