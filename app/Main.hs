{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DerivingStrategies   #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}

module Main where

import           Control.Monad        (void, (>=>))
import           Data.Aeson           (FromJSON (parseJSON), ToJSON (toJSON),
                                       decode, object, withObject, withText,
                                       (.:), (.=))
import qualified Data.ByteString.Lazy as LBS
import           Data.Coerce          (coerce)
import           Data.List            (intersperse, sort)
import           Data.List.NonEmpty   (NonEmpty)
import qualified Data.List.NonEmpty   as NonEmpty
import           Data.Maybe           (fromMaybe, mapMaybe)
import           Data.Ord             (comparing)
import           Data.Text            (Text)
import qualified Data.Text            as Text
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
import           Turtle               (Line, MonadIO (liftIO), Shell, UTCTime,
                                       die, inproc, lineToText, select, sh,
                                       textToLine, textToLines)

default (Text)

class ToLine a where
    toLine :: a -> Line

dmenuSelect :: [Text.Text] -> NonEmpty Line -> Shell Line
dmenuSelect args ls = inproc "dmenu" (["-l", Text.pack (show (length ls))] <> args) (select ls)

main :: IO ()
main = do
    manager <- newManager defaultManagerSettings
    baseUrl <- parseBaseUrl "http://localhost:8087"
    let env = mkClientEnv manager baseUrl
    sh $ do
        status <- getStatus env
        case statusDataStatus status of
            Locked -> do
                _pw <- askPassword
                _ <- login env (Password "")
                die "Logged in"
            Unlocked -> do
                items <- sort <$> getItems env
                let options = NonEmpty.fromList $ map toLine items <> otherActions
                selected <- dmenuSelect [] options
                liftIO $ print selected

data MetaAction = Cancel | LogOut
    deriving stock (Show, Eq, Enum, Bounded)

instance ToLine MetaAction where
    toLine = \case
        LogOut -> "Lock vault"
        Cancel -> "Cancel"

otherActions :: [Line]
otherActions = map toLine [minBound :: MetaAction ..]

handleClientError :: ClientError -> Shell a
handleClientError clientError = case clientError of
    (DecodeFailure df _)           -> announce "Decode failure" >> die df
    (ConnectionError _)            -> logId "Connection error"
    (UnsupportedContentType _ res) -> log "Unsupported content type" res
    (InvalidContentTypeHeader res) -> log "Invalid content type header" res
    (FailureResponse _ res)        -> logId (reason res)
  where
    reason :: Response -> Text
    reason =
        fromMaybe "Something went wrong"
            . coerce
            . decode @VaultFailureResponse
            . responseBody

    logId :: Text -> Shell a
    logId err = announce err >> die err

    log :: Text -> Response -> Shell a
    log err full = announce err >> die (err <> ":" <> tshow' (responseBody full))

tshow' :: LBS.ByteString -> Text
tshow' = Text.pack . show

askPassword :: Shell Password
askPassword = Password . lineToText <$> inproc "dmenu" args ""
  where
    obscureColor = "#222222"
    args = ["-p", "Enter Password ", "-nb", obscureColor, "-nf", obscureColor]

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

getItems :: ClientEnv -> Shell [ItemTemplate]
getItems env =
    liftIO (runClientM items env) >>= \case
        Left e -> handleClientError e
        Right (VaultResponse res) -> pure (coerce res)
  where
    items = vaultClient // itemsEp // getItemsEp

getStatus :: ClientEnv -> Shell StatusData
getStatus env =
    liftIO (runClientM status env) >>= \case
        Left e -> handleClientError e
        Right (VaultResponse res) -> pure res
  where
    status = vaultClient // miscEp // statusEp

announce :: Text -> Shell ()
announce msg = void $ dmenuSelect [] (textToLines msg)

type Todo = PostNoContent

data VaultApi as = VaultApi
    { lockingEp :: as :- NamedRoutes VaultLockApi
    , itemsEp   :: as :- NamedRoutes VaultItemsApi
    , miscEp    :: as :- NamedRoutes VaultMiscellaneousApi
    }
    deriving stock (Generic)

data VaultLockApi as = LockApi
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

newtype VaultMiscellaneousApi as = VaultMiscellaneousApi
    { statusEp :: as :- "status" :> Get '[JSON] (VaultResponse StatusData)
    }
    deriving stock (Generic)

data StatusData = StatusData
    { statusDataServerUrl :: Maybe Text
    , statusDataLastSync  :: UTCTime
    , statusDataUserEmail :: Text
    , statusDataUserId    :: Text
    , statusDataStatus    :: LockStatus
    }
    deriving stock (Show, Eq)

data LockStatus = Locked | Unlocked
    deriving stock (Show, Eq)

instance FromJSON LockStatus where
    parseJSON = withText "LockStatus" $ \case
        "locked" -> pure Locked
        "unlocked" -> pure Unlocked
        s -> fail ("Invalid lock status: " <> Text.unpack s)

instance FromJSON StatusData where
    parseJSON =
        withObject "StatusData" $
            (.: "template") >=> \t -> do
                statusDataServerUrl <- t .: "serverUrl"
                statusDataLastSync <- t .: "lastSync"
                statusDataUserEmail <- t .: "userEmail"
                statusDataUserId <- t .: "userId"
                statusDataStatus <- t .: "status"
                pure StatusData{..}

newtype ItemsData = ItemsData
    { unItemsData :: [ItemTemplate]
    }
    deriving stock (Show, Eq)

instance FromJSON ItemsData where
    parseJSON = withObject "ItemsData" $ \o -> ItemsData <$> o .: "data"

data Item
    = ItemLogin Login
    | ItemCard Card
    | ItemSecureNote SecureNote
    | ItemIdentity Identity
    deriving stock (Show, Eq, Ord)

toEmoji :: Item -> Text
toEmoji = \case
    ItemLogin _ -> "🔐"
    ItemCard _ -> "💳"
    ItemIdentity _ -> "👤"
    ItemSecureNote _ -> "🗒️"

instance ToLine ItemTemplate where
    toLine (ItemTemplate _ name _ item) = merge $ mapMaybe textToLine $ mappend [toEmoji item, name] $ case item of
        ItemLogin Login{..} -> [loginUsername] <> map unUri loginUris
        ItemCard Card{..}   -> [Text.take 5 cardNumber <> "..."]
        ItemIdentity _      -> ["Identity"]
        ItemSecureNote _    -> ["Secure Note"]

merge :: [Line] -> Line
merge = mconcat . intersperse " - "

safeHead :: [a] -> Maybe a
safeHead []      = Nothing
safeHead (x : _) = Just x

data ItemTemplate = ItemTemplate
    { itfolderId :: Maybe Text
    , itName     :: Text
    , itNotes    :: Maybe Text
    , itItem     :: Item
    }
    deriving (Show, Eq)

instance Ord ItemTemplate where
    compare = comparing itItem

newtype Uri = Uri
    { unUri :: Text
    }
    deriving stock (Show, Eq, Ord)

instance FromJSON Uri where
    parseJSON = withObject "Uri" $ \o -> Uri <$> o .: "uri"

data Login = Login
    { loginUris     :: [Uri]
    , loginUsername :: Text
    , loginPassword :: Text
    }
    deriving stock (Show, Eq, Ord)

instance FromJSON Login where
    parseJSON = withObject "Login" $ \o ->
        Login <$> o .: "uris" <*> o .: "username" <*> o .: "password"

newtype SecureNote = SecureNote
    { secureNoteType :: Int
    }
    deriving stock (Show, Eq, Ord)

instance FromJSON SecureNote where
    parseJSON = withObject "SecureNote" $ \o -> SecureNote <$> o .: "type"

data Card = Card
    { cardHoleName :: Text
    , cardNumber   :: Text
    , cardCode     :: Text
    }
    deriving stock (Show, Eq, Ord)

instance FromJSON Card where
    parseJSON = withObject "Card" $ \o ->
        Card <$> o .: "cardholderName" <*> o .: "number" <*> o .: "code"

data Identity = Identity {}
    deriving stock (Show, Eq, Ord)

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
