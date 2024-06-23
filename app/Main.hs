{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DerivingStrategies   #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}

module Main where

import           Data.Aeson          (FromJSON (parseJSON), ToJSON (toJSON),
                                      decode, object, withObject, withText,
                                      (.:), (.=))
import           Data.Coerce         (coerce)
import           Data.List           (intersperse, sort)
import           Data.List.NonEmpty  (NonEmpty)
import qualified Data.List.NonEmpty  as NonEmpty
import qualified Data.Map            as Map
import           Data.Maybe          (fromMaybe, mapMaybe)
import           Data.Ord            (comparing)
import           Data.Text           (Text)
import qualified Data.Text           as Text
import qualified Data.Text.IO
import           GHC.Generics        (Generic)
import           Network.HTTP.Client (defaultManagerSettings, newManager)
import           Prelude             hiding (log)
import           Servant             (Get, JSON, NamedRoutes, Post,
                                      PostNoContent, Proxy (Proxy), ReqBody,
                                      (:-), (:>))
import           Servant.Client      (AsClientT, ClientEnv, ClientError (..),
                                      ClientM, Response, client, mkClientEnv,
                                      parseBaseUrl, responseBody, runClientM,
                                      (//))
import           Turtle              (IsString (fromString), Line,
                                      MonadIO (liftIO), Shell, UTCTime, die,
                                      inproc, lineToText, linesToText, select,
                                      sh, textToLine, textToLines,
                                      unsafeTextToLine, void, when, (>=>))

default (Text)

class ToLine a where
    toLine :: a -> Line

dmenuSelect :: [Text.Text] -> Text -> NonEmpty Line -> Shell Line
dmenuSelect args p ls = inproc "dmenu" (["-i", "-l", Text.pack (show (min 24 (length ls))), "-p", p] <> args) (select ls)

main :: IO ()
main = do
    [pass] <- Text.lines <$> Data.Text.IO.readFile ".env.development"
    manager <- newManager defaultManagerSettings
    baseUrl <- parseBaseUrl "http://localhost:8087"
    let env = mkClientEnv manager baseUrl
    sh $ do
        -- status <- getStatus env
        --
        -- when (statusDataStatus status == Locked) $
        --     void $
        --         askPassword >> login env (Password pass)

        items <- getItemsMock env

        let widest = length (show (length items))
        let toEntry (i, item) = unsafeTextToLine (Text.pack (replicate (widest - length (show i)) ' ' <> show i <> ". ")) <> toLine item
        let items' = [toEntry e | e <- [1 :: Int ..] `zip` items]
        let fromEntry = (items !!) . subtract 1 . read . Text.unpack . head . Text.splitOn "." . lineToText

        let options = NonEmpty.fromList $ otherActions <> items'
        selected <- dmenuSelect [] "Entries" options

        case parseVaultAction (lineToText selected) of
            _ -> undefined

--            Just Sync   -> die "Synced"
--            Just LogOut -> die "Logged out" -- logout env >> die "Logged out"
--            Nothing     -> presentItemOptions (fromEntry selected)

presentItemOptions :: ItemTemplate -> Shell Line
presentItemOptions = dmenuSelect [] "" . NonEmpty.fromList . ("Back" :) . map toLine . itemOptions

paste :: Text -> Shell ()
paste text = void $ inproc "xdotool" ["type", text] ""

performItemAction :: ItemAction -> Shell ()
performItemAction (Paste _ info) = paste info

itemOptions :: ItemTemplate -> [ItemAction]
itemOptions it = case itItem it of
    ItemLogin (Login _ name pw) -> [Paste "username" name, Paste "password" pw]
    ItemCard (Card _ number code) -> [Paste "card number" number, Paste "CSV" code]
    ItemSecureNote (SecureNote note) -> [Paste "note" (Text.pack $ show note)]
    ItemIdentity _ident -> []

data View = DashboardView [VaultAction] [ItemTemplate] | ItemView ItemTemplate

presentOptions :: View -> Shell Line
presentOptions (DashboardView actions items) = dmenuSelect [] "Entries" options

parseVaultAction :: Text -> Maybe VaultAction
parseVaultAction = (`Map.lookup` actions)
  where
    actions :: Map.Map Text VaultAction
    actions =
        Map.fromList $
            zip (map (lineToText . toLine @VaultAction) [minBound ..]) [minBound ..]

data MetaAction = Back
    deriving stock (Show, Eq)

instance ToLine MetaAction where
    toLine Back = "Back"

data VaultAction = Sync | LogOut
    deriving stock (Show, Eq, Enum, Bounded)

instance ToLine VaultAction where
    toLine LogOut = "Lock Vault"
    toLine Sync   = "Sync"

data ItemAction = Paste Label Text
    deriving stock (Show, Eq)

instance ToLine ItemAction where
    toLine (Paste lbl _) = "Paste " <> unsafeTextToLine (unLabel lbl)

newtype Label = Label {unLabel :: Text}
    deriving stock (Show, Eq)

instance IsString Label where
    fromString = Label . Text.pack

data Action = Meta MetaAction | Vault VaultAction | Item ItemAction
    deriving stock (Show, Eq)

otherActions :: [Line]
otherActions = map (toLine @VaultAction) [minBound ..]

{-
Handle errors that can occur when making requests to the vault server.
It will display a message in dmenu and print the error message to the console
and then exit the program.
-}
handleClientError :: ClientError -> Shell a
handleClientError clientError =
    case clientError of
        (DecodeFailure df _)           -> dmenuShow "Decode failure" >> pure df
        (ConnectionError _)            -> logId "Connection error"
        (UnsupportedContentType _ res) -> log "Unsupported content type" res
        (InvalidContentTypeHeader res) -> log "Invalid content type header" res
        (FailureResponse _ res)        -> logId (reason res)
        >>= die
  where
    reason :: Response -> Text
    reason =
        fromMaybe "Something went wrong"
            . coerce
            . decode @VaultFailureResponse
            . responseBody

    logId :: Text -> Shell Text
    logId err = dmenuShow err >> pure err

    log :: Text -> Response -> Shell Text
    log err full = dmenuShow err >> pure (err <> ":" <> Text.pack (show (responseBody full)))

askPassword :: Shell Password
askPassword = Password . lineToText <$> inproc "dmenu" args ""
  where
    obscureColor = "#222222"
    args = ["-p", "Enter Password ", "-nb", obscureColor, "-nf", obscureColor]

login :: ClientEnv -> Password -> Shell UnlockData
login env pw =
    callApi (unlock pw) env >>= \res ->
        dmenuShow (unlockDataTitle res)
            >> pure res
  where
    unlock = vaultClient // lockingEp // unlockEp

logout :: ClientEnv -> Shell ()
logout env = callApi lock env >>= \res -> dmenuShow (unLockDataTitle res)
  where
    lock = vaultClient // lockingEp // lockEp

getItems :: ClientEnv -> Shell [ItemTemplate]
getItems = coerce . callApi (vaultClient // itemsEp // getItemsEp)

getItemsMock :: ClientEnv -> Shell [ItemTemplate]
getItemsMock _ =
    pure $
        map
            ( ( \i ->
                    ItemTemplate
                        { itfolderId = Just $ "folder " <> i
                        , itName = "name " <> i
                        , itNotes = Just $ "note " <> i
                        , itItem =
                            ItemLogin
                                ( Login
                                    { loginUris = [Uri $ "https://somthing.se/" <> i]
                                    , loginUsername = "username " <> i
                                    , loginPassword = "password " <> i
                                    }
                                )
                        }
              )
                . Text.pack
                . show @Int
            )
            [1 .. 30]

getStatus :: ClientEnv -> Shell StatusData
getStatus = callApi (vaultClient // miscEp // statusEp)

callApi :: ClientM (VaultResponse b) -> ClientEnv -> Shell b
callApi action env =
    liftIO (runClientM action env) >>= \case
        Left e -> handleClientError e
        Right (VaultResponse res) -> pure res

dmenuShow :: Text -> Shell ()
dmenuShow msg = void $ dmenuSelect [] "" (textToLines msg)

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
    parseJSON = withObject "VaultFailureResponse" $ \o ->
        VaultFailureResponse <$> o .: "message"

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
    ItemLogin _ -> "(l)"
    ItemCard _ -> "(c)"
    ItemIdentity _ -> "(i)"
    ItemSecureNote _ -> "(s)"

instance ToLine ItemTemplate where
    toLine (ItemTemplate _ name _ item) = merge $
        mapMaybe textToLine $
            mappend [toEmoji item, name] $ case item of
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
    compare = comparing itItem <> comparing itName

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
