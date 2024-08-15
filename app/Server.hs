{-# LANGUAGE OverloadedStrings #-}

module Server (actOnStatus, serverStatus) where

import           Api
import           Control.Concurrent         (putMVar, threadDelay, tryTakeMVar)
import           Control.Concurrent.Async   (async, wait, withAsync)
import           Control.Concurrent.MVar    (MVar)
import           Control.Monad              (void, when)
import           Control.Monad.Trans.Reader (ask, asks, mapReaderT, runReaderT)
import           Data.Aeson                 (FromJSON, decode, parseJSON,
                                             withObject, (.:))
import           Data.ByteString.Lazy       as BL
import           Data.Coerce                (coerce)
import qualified Data.Text.Encoding         as T
import           Servant.Client             (ClientEnv, runClientM, (//))
import           Shelly                     (errorExit, liftIO, print_stdout,
                                             run, run_, shelly, silently,
                                             tracing)
import           System.Timeout             (timeout)

import           Control.Monad.Trans.Class  (MonadTrans (lift))
import           Data.Text                  (Text)
import           Env                        (App, Env (..))
import           Interaction
import           Menu                       (Arg (..), Menu (..), Prompt (..))
import           Prelude                    hiding (log)

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

actOnStatus :: MVar Password -> ServerStatus -> App ()
actOnStatus mpw status = do
    env <- ask
    case status of
        ServerOffline Undecided -> announce "Unexpected status response from bitwarden"
        ServerOffline Unauthenticated -> do
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
            mstatus <- bwServe
            liftIO $ print mstatus
            case mstatus of
                Just s  -> actOnStatus mpw s
                Nothing -> announce "Failed to start"
        ServerOnline Unlocked -> getItems >>= interactionLoop . dashboardI
        ServerOnline Locked -> do
            mpw' <- liftIO (tryTakeMVar mpw)
            case mpw' of
                Just pw -> void (unlock pw)
                Nothing -> mapReaderT silently $ do
                    interactionLoop unlockI

            items <- getItems
            interactionLoop (dashboardI items)

bwServe :: App (Maybe ServerStatus)
bwServe = do
    client <- asks envClient
    liftIO $
        timeout 10_000_000 $
            withAsync (pollServer client) $ \poll -> do
                void $ async (shelly $ run_ "bw" ["serve"])
                wait poll
  where
    pollServer :: ClientEnv -> IO ServerStatus
    pollServer client = do
        res <- fmap unVaultResponse <$> runClientM (vaultClient // miscEp // statusEp) client
        case res of
            Left _  -> threadDelay 50_000 >> pollServer client
            Right s -> pure (ServerOnline (statusLockStatus s))
