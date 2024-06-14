{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DerivingStrategies   #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE OverloadedStrings    #-}

module Main where

import           Control.Exception   (IOException, catch)
import           Control.Monad       (void)
import           Data.Aeson          (FromJSON (parseJSON), ToJSON (toJSON),
                                      decode, object, withObject, (.:), (.=))
import           Data.Coerce         (coerce)
import           Data.List.NonEmpty  (NonEmpty)
import qualified Data.List.NonEmpty  as NonEmpty
import           Data.Maybe          (fromMaybe)
import           Data.Text           (Text)
import qualified Data.Text           as Text
import qualified Data.Text.IO        as Text.IO
import           GHC.Generics        (Generic)
import           Network.HTTP.Client (defaultManagerSettings, newManager)
import           Servant             (JSON, NamedRoutes, Post, PostNoContent,
                                      Proxy (Proxy), ReqBody, (:-), (:>))
import           Servant.Client      (AsClientT, ClientEnv, ClientError (..),
                                      ClientM, Response, client, mkClientEnv,
                                      parseBaseUrl, responseBody, runClientM,
                                      (//))
import           Turtle              (Line, MonadIO (liftIO), Shell, die, echo,
                                      inproc, lineToText, output, select, sh,
                                      textToLines, unsafeTextToLine)
import           Turtle.Line         (textToLine)

default (Text)

dmenu :: [Text.Text] -> Shell Line -> Shell Line
dmenu = inproc "dmenu"

main :: IO ()
main = do
    manager <- newManager defaultManagerSettings
    baseUrl <- parseBaseUrl "http://localhost:8087"
    let env = mkClientEnv manager baseUrl
    sh $ do
        pw <- askPassword
        (UnlockData title key) <- login env pw
        case textToLine key of
            Nothing -> die "Failed to parse key"
            Just key' -> output "/home/ola/.hwarden" (pure key') >> echo (unsafeTextToLine title) >> echo key'

handleClientError :: ClientError -> Shell a
handleClientError clientError = case clientError of
    (DecodeFailure _ _)          -> msgAndExit "Failed to decode response"
    (UnsupportedContentType _ _) -> msgAndExit "Unsupported content type"
    (ConnectionError _)          -> msgAndExit "Connection error"
    (InvalidContentTypeHeader _) -> msgAndExit "Invalid content type header"
    (FailureResponse _ res)      -> msgAndExit (reason res)
  where
    reason :: Response -> Text
    reason =
        fromMaybe "Something went wrong"
            . coerce
            . decode @VaultFailureResponse
            . responseBody

    msgAndExit :: Text -> Shell a
    msgAndExit err = displayMessage err >> die err

askPassword :: Shell Password
askPassword = Password . lineToText <$> dmenu args ""
  where
    obscureColor = "#222222"
    args = ["-p", "Password: ", "-nb", obscureColor, "-nf", obscureColor]

login :: ClientEnv -> Password -> Shell UnlockData
login env pw =
    liftIO (runClientM (unlock pw) env) >>= \case
        Left e -> handleClientError e
        Right (VaultResponse res) ->
            displayMessage (unlockDataTitle res)
                >> pure res
  where
    unlock = vaultClient // lockingEp // unlockEp

displayMessage :: Text -> Shell ()
displayMessage msg = void $ dmenu ["-l", tshow (NonEmpty.length msgLines)] (select (NonEmpty.toList msgLines))
  where
    msgLines :: NonEmpty Line
    msgLines = textToLines msg

tshow :: (Show a) => a -> Text
tshow = Text.pack . show

getSessionKey :: FilePath -> IO (Maybe Text)
getSessionKey fp = (Just <$> Text.IO.readFile fp) `catch` handler
  where
    handler :: IOException -> IO (Maybe Text)
    handler _ = pure Nothing

type Todo = PostNoContent

newtype VaultApi as = VaultApi
    { lockingEp :: as :- NamedRoutes LockApi
    -- , itemsEp   :: as :- NamedRoutes VaultItemsApi
    }
    deriving stock (Generic)

data LockApi as = LockApi
    { lockEp :: as :- "lock" :> Post '[JSON] (VaultResponse LockData)
    , unlockEp :: as :- "unlock" :> ReqBody '[JSON] Password :> Post '[JSON] (VaultResponse UnlockData)
    }
    deriving stock (Generic)

vaultClient :: VaultApi (AsClientT ClientM)
vaultClient = client (Proxy @(NamedRoutes VaultApi))

newtype VaultResponse a = VaultResponse {unVaultResponse :: a}
    deriving stock (Show, Eq)

newtype VaultFailureResponse = VaultFailureResponse
    { unVaultFailureResponse :: Text
    }
    deriving stock (Show, Eq)

instance FromJSON VaultFailureResponse where
    parseJSON = withObject "VaultFailureResponse" $ \o -> VaultFailureResponse <$> o .: "message"

instance (FromJSON a) => FromJSON (VaultResponse a) where
    parseJSON = withObject "VaultResponse" $ \o -> do
        success <- o .: "success"
        if success
            then VaultResponse <$> o .: "data"
            else fail "Unsuccessful response"

data UnlockData = UnlockData
    { unlockDataTitle      :: Text
    , unlockDataSessionKey :: Text
    }
    deriving stock (Show, Eq)

instance FromJSON UnlockData where
    parseJSON = withObject "UnlockData" $ \o ->
        UnlockData <$> o .: "title" <*> o .: "raw"

newtype LockData = LockData
    { lockDataTitle :: Text
    }
    deriving stock (Show, Eq)

instance FromJSON LockData where
    parseJSON = withObject "LockData" $ \o -> LockData <$> o .: "title"

newtype Password = Password
    { unPassword :: Text
    }
    deriving stock (Show, Eq)

instance ToJSON Password where
    toJSON (Password pass) = object ["password" .= pass]

-- data VaultItemsApi as = VaultItemsApi
--     { addItemEp     :: as :- Todo
--     , editItemEp    :: as :- Todo
--     , getItemEp     :: as :- Todo
--     , getItemsEp    :: as :- Todo
--     , deleteItemEp  :: as :- Todo
--     , restoreItemEp :: as :- Todo
--     }
--     deriving stock (Generic)
--
-- data MiscellaneousApi as = MiscellaneousApi
--     { syncEp           :: as :- Todo
--     , statusEp         :: as :- Todo
--     , generatePassword :: as :- Todo
--     }
--     deriving stock (Generic)
