{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DerivingStrategies   #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}

module Main where

import           Control.Exception    (IOException, catch)
import           Control.Monad        (void)
import           Data.Aeson           (FromJSON (parseJSON), ToJSON (toJSON),
                                       decode, object, withObject, (.:), (.=))
import qualified Data.ByteString.Lazy as LBS
import           Data.Coerce          (coerce)
import           Data.List.NonEmpty   (NonEmpty)
import qualified Data.List.NonEmpty   as NonEmpty
import           Data.Maybe           (fromMaybe)
import           Data.Text            (Text)
import qualified Data.Text            as Text
import qualified Data.Text.IO         as Text.IO
import           GHC.Generics         (Generic)
import           Network.HTTP.Client  (defaultManagerSettings, newManager)
import           Prelude              hiding (log)
import           Servant              (Get, JSON, NamedRoutes, Post,
                                       PostNoContent, Proxy (Proxy), ReqBody,
                                       (:-), (:>))
import           Servant.Client       (AsClientT, ClientEnv, ClientError (..),
                                       ClientM, Response, client, mkClientEnv,
                                       parseBaseUrl, responseBody, runClientM,
                                       (//))
import           Turtle               (Line, MonadIO (liftIO), Shell, die, echo,
                                       export, inproc, lineToText, select, sh,
                                       textToLines, unsafeTextToLine, (<&>))
import           Turtle.Line          (textToLine)

default (Text)

dmenu :: [Text.Text] -> Shell Line -> Shell Line
dmenu = inproc "dmenu"

main :: IO ()
main = do
    manager <- newManager defaultManagerSettings
    baseUrl <- parseBaseUrl "http://localhost:8087"
    let env = mkClientEnv manager baseUrl
    sh $ do
        _pw <- askPassword
        (UnlockData _title key) <- login env (Password "")
        case textToLine key of
            Nothing   -> die "Failed to parse key"
            Just key' -> echo "" -- export "BW_SESSION" (lineToText key')
        items <- coerce (getItems env)
        let options = NonEmpty.fromList $ map itemToLine items <> ["Log out"]
        dmenuSelect options >>= \case
            "Log out" -> logout env <&> dmenuSelect . pure . unsafeTextToLine . coerce
            something -> inproc "dunstify" ["-t", "1000", lineToText something] "" >> die (lineToText something)

handleClientError :: ClientError -> Shell a
handleClientError clientError = case clientError of
    (DecodeFailure df _)           -> announce "Decode failure" *> die df
    (ConnectionError _)            -> (announce *> die) "Connection error"
    (UnsupportedContentType _ res) -> log "Unsupported content type" res
    (InvalidContentTypeHeader res) -> log "Invalid content type header" res
    (FailureResponse _ res)        -> (announce *> die) (reason res)
  where
    reason :: Response -> Text
    reason =
        fromMaybe "Something went wrong"
            . coerce
            . decode @VaultFailureResponse
            . responseBody

    log :: Text -> Response -> Shell a
    log err full = announce err >> die (err <> ":" <> tshow' (responseBody full))

tshow' :: LBS.ByteString -> Text
tshow' = Text.pack . show

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
            announce (unlockDataTitle res)
                >> pure res
  where
    unlock = vaultClient // lockingEp // unlockEp

logout :: ClientEnv -> Shell LockData
logout env =
    liftIO (runClientM lock env) >>= \case
        Left e -> handleClientError e
        Right (VaultResponse res) ->
            announce (unLockDataTitle res)
                >> pure res
  where
    lock = vaultClient // lockingEp // lockEp

getItems :: ClientEnv -> Shell ItemsData
getItems env =
    liftIO (runClientM items env) >>= \case
        Left e -> handleClientError e
        Right (VaultResponse res) -> pure res
  where
    items = vaultClient // itemsEp // getItemsEp

dmenuSelect :: NonEmpty Line -> Shell Line
dmenuSelect ls = dmenu ["-l", tshow (length ls)] (select ls)

announce :: Text -> Shell ()
announce msg = void $ dmenuSelect (textToLines msg)

tshow :: (Show a) => a -> Text
tshow = Text.pack . show

getSessionKey :: FilePath -> IO (Maybe Text)
getSessionKey fp = (Just <$> Text.IO.readFile fp) `catch` handler
  where
    handler :: IOException -> IO (Maybe Text)
    handler _ = pure Nothing

itemToLine :: ItemTemplate -> Line
itemToLine = unsafeTextToLine . itName

type Todo = PostNoContent

data VaultApi as = VaultApi
    { lockingEp :: as :- NamedRoutes LockApi
    , itemsEp   :: as :- NamedRoutes VaultItemsApi
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

newtype SessionKey = SessionKey Line

data UnlockData = UnlockData
    { unlockDataTitle      :: Text
    , unlockDataSessionKey :: Text
    }
    deriving stock (Show, Eq)

instance FromJSON UnlockData where
    parseJSON = withObject "UnlockData" $ \o ->
        UnlockData <$> o .: "title" <*> o .: "raw"

newtype LockData = LockData
    { unLockDataTitle :: Text
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

data VaultItemsApi as = VaultItemsApi
    { addItemEp :: as :- Todo
    , editItemEp :: as :- Todo
    , getItemEp :: as :- Todo
    , getItemsEp :: as :- "list" :> "object" :> "items" :> Get '[JSON] (VaultResponse ItemsData)
    , deleteItemEp :: as :- Todo
    , restoreItemEp :: as :- Todo
    }
    deriving stock (Generic)

newtype ItemsData = ItemsData
    { unItemsData :: [ItemTemplate]
    }
    deriving stock (Show, Eq)

instance FromJSON ItemsData where
    parseJSON = withObject "ItemsData" $ \o -> ItemsData <$> o .: "data"

data Item
    = ItemLogin Login
    | ItemCard Card
    | ItemIdentity Identity
    | ItemSecureNote SecureNote
    deriving stock (Show, Eq)

data ItemTemplate = ItemTemplate
    { itfolderId :: Maybe Text
    , itName     :: Text
    , itNotes    :: Maybe Text
    , itItem     :: Item
    }
    deriving (Show, Eq)

newtype Uri = Uri
    { unUri :: Text
    }
    deriving stock (Show, Eq)

instance FromJSON Uri where
    parseJSON = withObject "Uri" $ \o -> Uri <$> o .: "uri"

data Login = Login
    { loginUris     :: [Uri]
    , loginUsername :: Text
    , loginPassword :: Text
    }
    deriving stock (Show, Eq)

instance FromJSON Login where
    parseJSON = withObject "Login" $ \o ->
        Login <$> o .: "uris" <*> o .: "username" <*> o .: "password"

data SecureNote = SecureNote
    { secureNoteType :: Int
    }
    deriving stock (Show, Eq)

instance FromJSON SecureNote where
    parseJSON = withObject "SecureNote" $ \o -> SecureNote <$> o .: "type"

data Card = Card
    { cardHoleName :: Text
    , cardNumber   :: Text
    , cardCode     :: Text
    }
    deriving stock (Show, Eq)

instance FromJSON Card where
    parseJSON = withObject "Card" $ \o ->
        Card <$> o .: "cardholderName" <*> o .: "number" <*> o .: "code"

data Identity = Identity {}
    deriving stock (Show, Eq)

instance FromJSON Identity where
    parseJSON = withObject "Identity" $ const (pure Identity)

instance FromJSON ItemTemplate where
    parseJSON = withObject "ItemTemplate" $ \o -> do
        itType :: Int <- o .: "type"
        itFolderId <- o .: "folderId"
        itName <- o .: "name"
        itNotes <- o .: "notes"
        let item = ItemTemplate itFolderId itName itNotes
        case itType of
            1 -> item . ItemLogin <$> o .: "login"
            2 -> item . ItemSecureNote <$> o .: "secureNote"
            3 -> item . ItemCard <$> o .: "card"
            4 -> item . ItemIdentity <$> o .: "identity"
            _ -> fail "Invalid item type"
