{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DerivingStrategies   #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}

module Main where

import           Control.Monad       (void, when, (>=>))
import           Data.Aeson          (FromJSON (parseJSON), ToJSON (toJSON),
                                      decode, object, withObject, withText,
                                      (.:), (.:?), (.=))
import           Data.Coerce         (coerce)
import           Data.List           (intersperse)
import qualified Data.Map            as Map
import           Data.Maybe          (catMaybes, fromMaybe)
import           Data.Ord            (comparing)
import           Data.Text           (Text)
import qualified Data.Text           as Text
import           Data.Time           (UTCTime)
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
import           Shelly              (Sh, errorExit, fromText, get_env, liftIO,
                                      run, run_, setStdin, shelly, toTextIgnore,
                                      verbosely, withTmpDir, writefile, (<.>))

default (Text)

class ToEntry a where
    toEntry :: a -> Text

dmenuSelect :: [Text.Text] -> Text -> Text -> Sh Text
dmenuSelect args p ls =
    setStdin ls
        >> run "dmenu" (["-i", "-l", Text.pack (show (min 24 (length (Text.lines ls)))), "-p", p] <> args)

main :: IO ()
main = do
    manager <- newManager defaultManagerSettings
    baseUrl <- parseBaseUrl "http://localhost:8087"
    let env = mkClientEnv manager baseUrl
    shelly $ do
        status <- getStatus env

        when (statusDataStatus status == Locked) $
            void $
                askPassword >>= \(Password p) -> login env (Password (head (Text.lines p)))

        items <- getItems env
        handleView env (DashboardView items)

paste :: Text -> Sh ()
paste text = xdotool ["type", "--delay", "0", text]

pressKey :: Text -> Sh ()
pressKey key = xdotool ["key", key]

xdotool :: [Text] -> Sh ()
xdotool = run_ "xdotool"

quickAction :: Item -> Sh ()
quickAction item = case item of
    ItemLogin (Login{..}) -> case (loginUsername, loginPassword) of
        (Just u, Just p) -> paste u >> pressKey "Tab" >> paste p >> pressKey "Return"
        _ -> void $ logId "No username or password"
    ItemCard (Card{..}) -> case (cardHolderName, cardNumber, cardCode) of
        (h, n, c) -> paste h >> paste " " >> paste n >> paste " " >> paste c
    ItemIdentity _ -> paste "identity"
    ItemSecureNote _ -> paste "note"

openInEditor :: Text -> Sh ()
openInEditor text = do
    editor <- get_env "EDITOR"
    term <- get_env "TERM"
    case editor of
        Nothing -> errorExit "EDITOR not set"
        Just e -> case term of
            Nothing -> errorExit "TERM not set"
            Just t -> withTmpDir $ \fp -> do
                writefile (fp <.> "hwarden") text
                run_ (fromText t) [e, toTextIgnore (fp <.> "hwarden")]

-- fp <- mktempfile "/tmp" "hwarden"
-- output fp text
-- case editor of
--     Nothing -> die "EDITOR not set"
--     Just e -> case term of
--         Nothing -> die "TERM not set"
--         Just t  -> void $ inproc t [e, Text.pack fp] ""

individualItemEntries :: ItemTemplate -> [Entry ()]
individualItemEntries it = case itItem it of
    ItemLogin l         -> loginActions l
    ItemCard c          -> cardActions c
    ItemSecureNote _    -> secureNoteActions it
    ItemIdentity _ident -> []
  where
    cardActions :: Card -> [Entry ()]
    cardActions (Card _ number code) =
        [ Entry Item ("CVV: " <> code) (paste code)
        , Entry Item ("Number: " <> number) (paste number)
        ]
    loginActions :: Login -> [Entry ()]
    loginActions (Login _ name pw) =
        catMaybes
            [ Entry Item <$> fmap ("Username: " <>) name <*> fmap paste name
            , Entry Item "Password: ********" . paste <$> pw
            ]
    secureNoteActions :: ItemTemplate -> [Entry ()]
    secureNoteActions item =
        let lines' = Text.lines $ fromMaybe "" (itNotes item)
         in catMaybes
                [ Entry
                    Item
                    ("Note: " <> head lines' <> " (" <> Text.pack (show $ length lines') <> " lines)")
                    . openInEditor
                    <$> itNotes item
                ]

data View = DashboardView [ItemTemplate] | ItemView ItemTemplate | AllItemsView [ItemTemplate]

entriesToMap :: [Entry a] -> Map.Map Text (Sh a)
entriesToMap = Map.fromList . map (\e -> (entryLabel e, entryRun e))

runSelected :: Map.Map Text (Sh a) -> Text -> Sh a
runSelected actions selected = case Map.lookup selected actions of
    Just action -> action
    Nothing     -> logId ("No such entry: " <> selected) >>= errorExit

handleDashboardView :: ClientEnv -> [ItemTemplate] -> Sh ()
handleDashboardView env items = do
    let allEntries = miscEntries <> itemEntries items (quickAction . itItem)
    let actions = entriesToMap allEntries
    dmenuSelect [] "Entries" (Text.unlines (map entryLabel allEntries))
        >>= runSelected actions . head . Text.lines
  where
    miscEntries :: [Entry ()]
    miscEntries =
        [ Entry Misc "View/Type individual entries" (handleAllItemsView env items)
        , Entry Misc "View previous entry (TODO)" (pure ())
        , Entry Misc "Edit entry (TODO)" (pure ())
        , Entry Misc "Add entry (TODO)" (pure ())
        , Entry Misc "Manage folders (TODO)" (pure ())
        , Entry Misc "Manage collections (TODO)" (pure ())
        , Entry Misc "Sync vault" (sync env)
        , Entry Misc "Switch vaults (TODO)" (sync env)
        , Entry Misc "Lock vault" (logout env)
        ]

handleAllItemsView :: ClientEnv -> [ItemTemplate] -> Sh ()
handleAllItemsView env items = do
    let allEntries = itemEntries items (handleView env . ItemView)
    let actions = entriesToMap allEntries
    dmenuSelect [] "Entries" (Text.unlines (map entryLabel allEntries))
        >>= runSelected actions . head . Text.lines

itemEntries :: [ItemTemplate] -> (ItemTemplate -> Sh ()) -> [Entry ()]
itemEntries xs action =
    [ Entry Item (toEntry' (i, x)) (action x)
    | (i, x) <- [1 :: Int ..] `zip` xs
    ]
  where
    toEntry' :: (Show a) => (ToEntry b) => (a, b) -> Text
    toEntry' (i, x) = Text.pack (spaces i <> show i <> ". ") <> toEntry x

    spaces i = replicate (width xs - length (show i)) ' '

    width :: [a] -> Int
    width = length . show . length

handleView :: ClientEnv -> View -> Sh ()
handleView env view = case view of
    DashboardView items -> handleDashboardView env items
    AllItemsView items  -> handleAllItemsView env items
    ItemView item       -> handleItemView item

handleItemView :: ItemTemplate -> Sh ()
handleItemView item =
    dmenuSelect [] "Entry" (Text.unlines (map entryLabel entries))
        >>= runSelected (entriesToMap entries)
  where
    entries = individualItemEntries item
data ActionLevel = Misc | Meta | Item
    deriving (Show, Eq, Ord)

data Entry a = Entry
    { entryLevel :: ActionLevel
    , entryLabel :: Text
    , entryRun   :: Sh a
    }

newtype Label = Label {unLabel :: Text}
    deriving stock (Show, Eq)

{-
Handle errors that can occur when making requests to the vault server.
It will display a message in dmenu and print the error message to the console
and then exit the program.
-}
handleClientError :: ClientError -> Sh a
handleClientError clientError =
    case clientError of
        (DecodeFailure df _)           -> dmenuShow "Decode failure" >> pure df
        (ConnectionError _)            -> logId "Connection error"
        (UnsupportedContentType _ res) -> log "Unsupported content type" res
        (InvalidContentTypeHeader res) -> log "Invalid content type header" res
        (FailureResponse _ res)        -> logId (reason res)
        >>= errorExit
  where
    reason :: Response -> Text
    reason =
        fromMaybe "Something went wrong"
            . coerce
            . decode @VaultFailureResponse
            . responseBody

logId :: Text -> Sh Text
logId err = dmenuShow err >> pure err

log :: Text -> Response -> Sh Text
log err full = dmenuShow err >> pure (err <> ":" <> Text.pack (show (responseBody full)))

askPassword :: Sh Password
askPassword = Password <$> run "dmenu" args
  where
    obscureColor = "#222222"
    args = ["-p", "Enter Password ", "-nb", obscureColor, "-nf", obscureColor]

login :: ClientEnv -> Password -> Sh UnlockData
login env pw =
    callApi (unlock pw) env >>= \res ->
        dmenuShow (unlockDataTitle res)
            >> pure res
  where
    unlock = vaultClient // lockingEp // unlockEp

logout :: ClientEnv -> Sh ()
logout env = callApi lock env >>= dmenuShow . unTitledData
  where
    lock = vaultClient // lockingEp // lockEp

sync :: ClientEnv -> Sh ()
sync env = callApi sync' env >>= dmenuShow . unTitledData
  where
    sync' = vaultClient // miscEp // syncEp

getItems :: ClientEnv -> Sh [ItemTemplate]
getItems = fmap coerce . callApi (vaultClient // itemsEp // getItemsEp)

getStatus :: ClientEnv -> Sh StatusData
getStatus = callApi (vaultClient // miscEp // statusEp)

callApi :: ClientM (VaultResponse a) -> ClientEnv -> Sh a
callApi action env =
    liftIO (runClientM action env) >>= \case
        Left e -> handleClientError e
        Right (VaultResponse res) -> pure res

dmenuShow :: Text -> Sh ()
dmenuShow = void . dmenuSelect [] ""

type Todo = PostNoContent

data VaultApi as = VaultApi
    { lockingEp :: as :- NamedRoutes VaultLockApi
    , itemsEp   :: as :- NamedRoutes VaultItemsApi
    , miscEp    :: as :- NamedRoutes VaultMiscellaneousApi
    }
    deriving stock (Generic)

data VaultLockApi as = LockApi
    { lockEp :: as :- "lock" :> Post '[JSON] (VaultResponse TitledData)
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

data UnlockData = UnlockData
    { unlockDataTitle      :: Text
    , unlockDataSessionKey :: Text
    }
    deriving stock (Show, Eq)

instance FromJSON UnlockData where
    parseJSON = withObject "UnlockData" $ \o ->
        UnlockData <$> o .: "title" <*> o .: "raw"

newtype TitledData = TitledData
    { unTitledData :: Text
    }
    deriving stock (Show, Eq)

instance FromJSON TitledData where
    parseJSON = withObject "TitledData" $ \o -> TitledData <$> o .: "title"

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

data VaultMiscellaneousApi as = VaultMiscellaneousApi
    { statusEp :: as :- "status" :> Get '[JSON] (VaultResponse StatusData)
    , syncEp   :: as :- "sync" :> Post '[JSON] (VaultResponse TitledData)
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
    toEntry (ItemTemplate _ name _ item) =
        mappend (toSymbol item <> " " <> name) $
            merge $
                case item of
                    ItemLogin itemLogin -> catMaybes [loginUsername itemLogin] <> maybe [] coerce (loginUris itemLogin)
                    ItemCard Card{..} -> [Text.take 5 cardNumber <> "..."]
                    ItemIdentity _ -> ["Identity"]
                    ItemSecureNote _ -> ["Secure Note"]

merge :: [Text] -> Text
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
    { cardHolderName :: Text
    , cardNumber     :: Text
    , cardCode       :: Text
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
