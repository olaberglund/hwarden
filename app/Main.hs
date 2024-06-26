{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DerivingStrategies   #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}

module Main where

import           Data.Aeson          (FromJSON (parseJSON), ToJSON (toJSON),
                                      decode, object, withObject, withText,
                                      (.:), (.:?), (.=))
import           Data.Coerce         (coerce)
import           Data.List           (intersperse)
import           Data.List.NonEmpty  (NonEmpty)
import qualified Data.List.NonEmpty  as NonEmpty
import qualified Data.Map            as Map
import           Data.Maybe          (catMaybes, fromMaybe, mapMaybe)
import           Data.Ord            (comparing)
import           Data.Text           (Text)
import qualified Data.Text           as Text
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
                                      inproc, lineToText, select, sh,
                                      textToLine, textToLines, toLines,
                                      unsafeTextToLine, void, when, (>=>))
import           Turtle.Prelude      (need)

default (Text)

class ToEntry a where
    toEntry :: a -> Line

dmenuSelect :: [Text.Text] -> Text -> NonEmpty Line -> Shell Line
dmenuSelect args p ls =
    inproc
        "dmenu"
        (["-i", "-l", Text.pack (show (min 24 (length ls))), "-p", p] <> args)
        (select ls)

main :: IO ()
main = do
    manager <- newManager defaultManagerSettings
    baseUrl <- parseBaseUrl "http://localhost:8087"
    let env = mkClientEnv manager baseUrl
    sh $ do
        status <- getStatus env

        when (statusDataStatus status == Locked) $
            void $
                askPassword >>= login env

        items <- getItems env
        handleView env (DashboardView items)

paste :: Text -> Shell ()
paste text = void $ inproc "xdotool" ["type", text] ""

openEditor :: Shell Line -> Shell ()
openEditor stdin = do
    editor <- need "EDITOR"
    case editor of
        Just e  -> void $ inproc e [] stdin
        Nothing -> die "EDITOR not set"

itemEntries :: ItemTemplate -> [Entry ()]
itemEntries it = case itItem it of
    ItemLogin l         -> loginActions l
    ItemCard c          -> cardActions c
    ItemSecureNote _    -> secureNoteActions it
    ItemIdentity _ident -> []
  where
    cardActions :: Card -> [Entry ()]
    cardActions (Card _ number code) =
        [ Entry Item ("CVV: " <> unsafeTextToLine code) (paste code)
        , Entry Item ("Number: " <> unsafeTextToLine number) (paste number)
        ]
    loginActions :: Login -> [Entry ()]
    loginActions (Login _ name pw) =
        catMaybes
            [ Entry Item <$> fmap (unsafeTextToLine . ("Username: " <>)) name <*> fmap paste name
            , Entry Item "Password: ********" . paste <$> pw
            ]
    secureNoteActions :: ItemTemplate -> [Entry ()]
    secureNoteActions item =
        catMaybes
            [ Entry
                Item
                ("Note: " <> NonEmpty.head (textToLines $ fromMaybe "" (itNotes item)))
                . openEditor
                . toLines
                . pure
                <$> itNotes item
            ]

data View = DashboardView [ItemTemplate] | ItemView ItemTemplate

entriesToMap :: [Entry a] -> Map.Map Line (Shell a)
entriesToMap = Map.fromList . map (\e -> (entryLabel e, entryRun e))

runSelected :: Map.Map Line (Shell a) -> Line -> Shell a
runSelected actions selected = case Map.lookup selected actions of
    Just action -> action
    Nothing     -> logId (lineToText $ "No such entry: " <> selected) >>= die

handleDashboardView :: ClientEnv -> [ItemTemplate] -> Shell ()
handleDashboardView env items = do
    let miscEntries = [Entry Misc "Log out" (logout env)]
    let allEntries = miscEntries <> entries items
    let actions = entriesToMap allEntries
    dmenuSelect [] "Entries" (NonEmpty.fromList (map entryLabel allEntries))
        >>= runSelected actions
  where
    entries :: [ItemTemplate] -> [Entry ()]
    entries xs =
        [ Entry Item (toEntry' (i, x)) (handleView env (ItemView x))
        | (i, x) <- [1 :: Int ..] `zip` xs
        ]
      where
        toEntry' :: (Show a) => (ToEntry b) => (a, b) -> Line
        toEntry' (i, x) = unsafeTextToLine (Text.pack $ spaces i <> show i <> ". ") <> toEntry x

        spaces i = replicate (width xs - length (show i)) ' '

        width :: [a] -> Int
        width = length . show . length

handleView :: ClientEnv -> View -> Shell ()
handleView env view = case view of
    DashboardView items -> handleDashboardView env items
    ItemView item       -> handleItemView item

handleItemView :: ItemTemplate -> Shell ()
handleItemView item =
    dmenuSelect [] "Entry" (NonEmpty.fromList (map entryLabel entries))
        >>= runSelected (entriesToMap entries)
  where
    entries = itemEntries item
data ActionLevel = Misc | Meta | Item
    deriving (Show, Eq, Ord)

data Entry a = Entry
    { entryLevel :: ActionLevel
    , entryLabel :: Line
    , entryRun   :: Shell a
    }

instance Eq (Entry a) where
    (Entry level1 label1 _) == (Entry level2 label2 _) =
        level1 == level2 && label1 == label2

instance Ord (Entry a) where
    compare = comparing entryLevel

newtype Label = Label {unLabel :: Text}
    deriving stock (Show, Eq)

instance IsString Label where
    fromString = Label . Text.pack

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
logout env = callApi lock env >>= dmenuShow . unLockDataTitle
  where
    lock = vaultClient // lockingEp // lockEp

getItems :: ClientEnv -> Shell [ItemTemplate]
getItems = coerce . callApi (vaultClient // itemsEp // getItemsEp)

getStatus :: ClientEnv -> Shell StatusData
getStatus = callApi (vaultClient // miscEp // statusEp)

callApi :: ClientM (VaultResponse b) -> ClientEnv -> Shell b
callApi action env =
    liftIO (runClientM action env) >>= \case
        Left e -> handleClientError e
        Right (VaultResponse res) -> pure res

dmenuShow :: Text -> Shell ()
dmenuShow = void . dmenuSelect [] "" . textToLines

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

toSymbol :: Item -> Text
toSymbol = \case
    ItemLogin _ -> "(l)"
    ItemCard _ -> "(c)"
    ItemIdentity _ -> "(i)"
    ItemSecureNote _ -> "(s)"

instance ToEntry ItemTemplate where
    toEntry (ItemTemplate _ name _ item) = merge $
        mapMaybe textToLine $
            mappend [toSymbol item, name] $ case item of
                ItemLogin itemLogin -> catMaybes [loginUsername itemLogin] <> maybe [] coerce (loginUris itemLogin)
                ItemCard Card{..} -> [Text.take 5 cardNumber <> "..."]
                ItemIdentity _ -> ["Identity"]
                ItemSecureNote _ -> ["Secure Note"]

merge :: [Line] -> Line
merge = mconcat . intersperse " - "

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
    { loginUris     :: Maybe [Uri]
    , loginUsername :: Maybe Text
    , loginPassword :: Maybe Text
    }
    deriving stock (Show, Eq, Ord)

instance FromJSON Login where
    parseJSON = withObject "Login" $ \o ->
        Login <$> o .:? "uris" <*> o .:? "username" <*> o .: "password"

data SecureNote = SecureNote
    deriving stock (Show, Eq, Ord)

instance FromJSON SecureNote where
    parseJSON = withObject "SecureNote" $ const (pure SecureNote)

data Card = Card
    { cardHoleName :: Text
    , cardNumber   :: Text
    , cardCode     :: Text
    }
    deriving stock (Show, Eq, Ord)

instance FromJSON Card where
    parseJSON = withObject "Card" $ \o ->
        Card <$> o .: "cardholderName" <*> o .: "number" <*> o .: "code"

data Identity = Identity
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
