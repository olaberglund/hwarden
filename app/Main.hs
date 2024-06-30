{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DerivingStrategies   #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}

module Main where

import           Control.Monad       (replicateM_, void, when, (>=>))
import           Data.Aeson          (FromJSON (parseJSON), ToJSON (toJSON),
                                      decode, object, withObject, withText,
                                      (.:), (.:?), (.=))
import           Data.Coerce         (coerce)
import           Data.Functor        (($>), (<&>))
import           Data.List           (find, intersperse)
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
                                      PostNoContent, Proxy (Proxy), QueryFlag,
                                      QueryParam, ReqBody, (:-), (:>))
import           Servant.Client      (AsClientT, ClientEnv, ClientError (..),
                                      ClientM, Response, client, mkClientEnv,
                                      parseBaseUrl, responseBody, runClientM,
                                      (//))
import           Shelly              (Sh, errorExit, fromText, get_env, liftIO,
                                      readfile, run, run_, setStdin, shelly,
                                      silently, toTextIgnore, withTmpDir,
                                      writefile, (<.>))
import           Text.Read           (readMaybe)
import qualified Text.Read           as Text

default (Text)

class ToEntry a where
    toEntry :: a -> Text

dmenuSelect :: [Text.Text] -> Text -> Text -> Sh Text
dmenuSelect args p ls =
    setStdin ls
        >> run "dmenu" (["-i", "-l", Text.pack (show (min 24 (length (Text.lines ls)))), "-p", p] <> args)
        <&> head . Text.lines

main :: IO ()
main = do
    manager <- newManager defaultManagerSettings
    baseUrl <- parseBaseUrl "http://localhost:8087"
    let env = mkClientEnv manager baseUrl
    shelly $ do
        status <- getStatus env

        when (statusDataStatus status == Locked) $
            void $
                askPassword >>= login env . Password . head . Text.lines . coerce

        items <- getItems env
        handleDashboardView env items

paste :: Text -> Sh ()
paste text = xdotool ["type", "--delay", "0", text]

pressKey :: Text -> Sh ()
pressKey key = xdotool ["key", key]

xdotool :: [Text] -> Sh ()
xdotool = run_ "xdotool"

dashboardAction :: ItemTemplate -> Sh ()
dashboardAction i = case itItem i of
    ItemIdentity _ -> paste "identity"
    ItemSecureNote _ -> case itNotes i of
        Just n  -> openInEditor_ n
        Nothing -> dmenuShow "No note"
    ItemCard (Card{..}) ->
        openInEditor_ (Text.unlines [cardHolderName, cardNumber, cardCode])
    ItemLogin (Login{..}) -> case (loginUsername, loginPassword) of
        (Just u, Just p) -> do
            paste u
            pressKey "Tab"
            paste p
            replicateM_ 2 (pressKey "Return")
        _ -> dmenuShow "No username or password"

openInEditor_ :: Text -> Sh ()
openInEditor_ = void . openInEditor

openInEditor :: Text -> Sh Text
openInEditor text = do
    editor <- get_env "EDITOR"
    case editor of
        Nothing -> errorExit "EDITOR not set"
        Just e -> withTmpDir $ \fp -> do
            writefile (fp <.> "hwarden") text
            run_ "st" [e, toTextIgnore (fp <.> "hwarden")]
            readfile (fp <.> "hwarden")

individualItemEntries :: ItemTemplate -> [Entry ()]
individualItemEntries it =
    commonEntries <> case itItem it of
        ItemLogin l         -> loginEntries l
        ItemCard c          -> cardEntries c
        ItemSecureNote _    -> noteEntries it
        ItemIdentity _ident -> []
  where
    commonEntries =
        [ Entry ("Name:  " <> itName it) (pure ())
        , Entry ("Folder: " <> orNone (itFolderId it)) (pure ())
        , Entry ("Notes: " <> orNone (itNotes it)) (pure ())
        ]

    orNone :: Maybe Text -> Text
    orNone = fromMaybe "None"

    cardEntries :: Card -> [Entry ()]
    cardEntries (Card _ number code) =
        [ Entry ("CVV: " <> code) (paste code)
        , Entry ("Number: " <> number) (paste number)
        ]

    orDoNothing = maybe (pure ())

    loginEntries :: Login -> [Entry ()]
    loginEntries (Login uris name pw totp) =
        [ Entry ("Username: " <> orNone name) (orDoNothing paste name)
        , Entry "Password: ********" (orDoNothing paste pw)
        , Entry ("TOTP: " <> orNone totp) (pure ())
        ]
            <> maybe
                []
                ( map
                    ( \(Uri uri, i) ->
                        Entry ("URI " <> Text.pack (show i) <> ": " <> uri) (run_ "firefox" [uri])
                    )
                )
                (zip <$> uris <*> pure [1 :: Int ..])
    noteEntries :: ItemTemplate -> [Entry ()]
    noteEntries item =
        let lines' = Text.lines $ fromMaybe "" (itNotes item)
         in catMaybes
                [ Entry
                    ("Note: " <> head lines' <> " (" <> Text.pack (show $ length lines') <> " lines)")
                    . openInEditor_
                    <$> itNotes item
                ]

individualEditItemEntries :: ClientEnv -> ItemTemplate -> [Entry ()]
individualEditItemEntries env it =
    commonEntries <> itemSpecificEntries
  where
    itemSpecificEntries :: [Entry ()]
    itemSpecificEntries = case itItem it of
        ItemLogin l -> loginEntries l
        _           -> undefined

    commonEntries =
        [ Entry ("Name:  " <> itName it) (dmenuSelect [] "Name" (itName it) >>= \t -> handleEditItemView env it{itName = t})
        , Entry
            ("Folder: " <> orNone (itFolderId it))
            ( getFolders env
                >>= \folders ->
                    dmenuSelect [] "Folder" (Text.unlines (map (unFolder . coerce) folders))
                        >>= \t -> handleEditItemView env it{itFolderId = Just t}
            )
        , Entry ("Notes: " <> maybe "<Enter to add>" (const "<Enter to edit>") (itNotes it)) (openInEditor (fromMaybe "" (itNotes it)) >>= \t -> handleEditItemView env it{itNotes = Just t})
        ]

    orNone :: Maybe Text -> Text
    orNone = fromMaybe "None"

    loginEntries :: Login -> [Entry ()]
    loginEntries l@(Login uris name _pw totp) =
        [ Entry ("Username: " <> orNone name) (dmenuSelect [] "Username" (orNone name) >>= \t -> handleEditItemView env it{itItem = ItemLogin l{loginUsername = Just t}})
        , Entry "Password: ********" (generatePassword env >>= \t -> handleEditItemView env it{itItem = ItemLogin l{loginPassword = Just t}})
        , Entry
            "URLs: <Enter to edit>"
            ( do
                url <- dmenuSelect [] "URL (type to add new)" (Text.unlines (maybe [] coerce uris))
                case find ((== url) . unUri) (fromMaybe [] uris) of
                    Just oldUrl -> dmenuSelect [] "URL" (coerce oldUrl) >>= \newUrl -> handleEditItemView env it{itItem = ItemLogin (l{loginUris = Just (Uri newUrl : filter (/= oldUrl) (fromMaybe [] uris))})}
                    Nothing -> handleEditItemView env it{itItem = ItemLogin (l{loginUris = Just (Uri url : fromMaybe [] uris)})}
            )
        , Entry ("TOTP: " <> orNone totp) (dmenuShow "TODO")
        ]

entriesToMap :: [Entry a] -> Map.Map Text (Sh a)
entriesToMap = Map.fromList . map (\e -> (entryLabel e, entryRun e))

runSelected :: Map.Map Text (Sh a) -> Text -> Sh a
runSelected actions selected = case Map.lookup selected actions of
    Just action -> action
    Nothing     -> showExit ("No such entry: " <> selected)

handleDashboardView :: ClientEnv -> [ItemTemplate] -> Sh ()
handleDashboardView env items = do
    let allEntries = miscEntries <> itemEntries items dashboardAction
    let actions = entriesToMap allEntries
    dmenuSelect [] "Entries" (Text.unlines (map entryLabel allEntries))
        >>= runSelected actions
  where
    miscEntries :: [Entry ()]
    miscEntries =
        [ Entry "View/Type individual entries" (handleAllItemsView items)
        , Entry "View previous entry" (readCache >>= itemWithId items >>= handleItemView)
        , Entry "Edit entry (TODO)" (handleEditItemsView env items)
        , Entry "Add entry (TODO)" (pure ())
        , Entry "Manage folders (TODO)" (pure ())
        , Entry "Manage collections (TODO)" (pure ())
        , Entry "Sync vault" (sync env)
        , Entry "Switch vaults (TODO)" (sync env)
        , Entry "Lock vault" (logout env)
        ]

withCacheFile :: (FilePath -> Sh a) -> Sh a
withCacheFile action = do
    maybeHome <- get_env "HOME"
    case maybeHome of
        Just home -> action (fromText $ home <> "/.cache/hwarden")
        Nothing   -> errorExit "HOME not set"

writeCache :: Text -> Sh ()
writeCache = withCacheFile . flip writefile

readCache :: Sh Text
readCache = withCacheFile readfile

itemWithId :: [ItemTemplate] -> Text -> Sh ItemTemplate
itemWithId items id' = case find ((== id') . itId) items of
    Just x  -> pure x
    Nothing -> showExit "Could not find the previous item" >>= errorExit

handleAllItemsView :: [ItemTemplate] -> Sh ()
handleAllItemsView items = do
    let allEntries = itemEntries items (\i -> writeCache (itId i) >> handleItemView i)
    let actions = entriesToMap allEntries
    dmenuSelect [] "Entries" (Text.unlines (map entryLabel allEntries))
        >>= runSelected actions

handleEditItemsView :: ClientEnv -> [ItemTemplate] -> Sh ()
handleEditItemsView env items = do
    let allEntries = itemEntries items (handleEditItemView env)
    dmenuSelect [] "Entries" (Text.unlines (map entryLabel allEntries))
        >>= runSelected (entriesToMap allEntries)

handleEditItemView :: ClientEnv -> ItemTemplate -> Sh ()
handleEditItemView env item = do
    dmenuSelect [] "Entry" (Text.unlines (map entryLabel entries))
        >>= runSelected (entriesToMap entries)
  where
    entries = individualEditItemEntries env item

itemEntries :: [ItemTemplate] -> (ItemTemplate -> Sh ()) -> [Entry ()]
itemEntries xs action =
    [ Entry (toEntry' (i, x)) (action x)
    | (i, x) <- [1 :: Int ..] `zip` xs
    ]
  where
    toEntry' :: (Show a) => (ToEntry b) => (a, b) -> Text
    toEntry' (i, x) = Text.pack (spaces i <> show i <> ". ") <> toEntry x

    spaces i = replicate (width xs - length (show i)) ' '

    width :: [a] -> Int
    width = length . show . length

handleItemView :: ItemTemplate -> Sh ()
handleItemView item =
    dmenuSelect [] "Entry" (Text.unlines (map entryLabel entries))
        >>= runSelected (entriesToMap entries)
  where
    entries = individualItemEntries item

data Entry a = Entry
    { entryLabel :: Text
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
        (DecodeFailure df _) -> dmenuShow "Decode failure" >> errorExit df
        (ConnectionError _) -> showExit "Connection error"
        (UnsupportedContentType _ res) -> showResponseExit "Unsupported content type" res
        (InvalidContentTypeHeader res) -> showResponseExit "Invalid content type header" res
        (FailureResponse _ res) -> showExit (reason res)
  where
    reason :: Response -> Text
    reason =
        fromMaybe "Something went wrong"
            . coerce
            . decode @VaultFailureResponse
            . responseBody

showExit :: Text -> Sh a
showExit t = dmenuShow t >> errorExit t

showResponseExit :: Text -> Response -> Sh a
showResponseExit err full =
    dmenuShow err
        >> errorExit (err <> ":" <> Text.pack (show (responseBody full)))

askPassword :: Sh Password
askPassword = silently $ Password . head . Text.lines <$> run "dmenu" args
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

generatePassword :: ClientEnv -> Sh Text
generatePassword env = do
    method <- dmenuSelect [] "Method" (Text.unlines ["Generate", "Manual"])
    case head (Text.lines method) of
        "Generate" -> do
            len <- dmenuSelect [] "Length" "20"
            special <- dmenuSelect [] "Include special?" (Text.unlines ["Yes", "No"])
            case readMaybe @Int (Text.unpack len) of
                Nothing -> showExit "Invalid length"
                Just len' -> do
                    let gen spec = callApi (generatePassword' (Just len') False False False spec) env <&> unGenerateData
                    case head (Text.lines special) of
                        "Yes" -> gen True
                        "No"  -> gen False
                        _     -> showExit "Please choose Yes or No"
        "Manual" -> dmenuSelect [] "Password" ""
        _ -> showExit "Please choose Generate or Manual"
  where
    generatePassword' :: Maybe Int -> Bool -> Bool -> Bool -> Bool -> ClientM (VaultResponse GenerateData)
    generatePassword' = vaultClient // miscEp // generatePasswordEp

getItems :: ClientEnv -> Sh [ItemTemplate]
getItems = fmap coerce . callApi (vaultClient // itemsEp // getItemsEp)

getStatus :: ClientEnv -> Sh StatusData
getStatus = callApi (vaultClient // miscEp // statusEp)

getFolders :: ClientEnv -> Sh [Folder]
getFolders = fmap coerce . callApi (vaultClient // foldersEp // getFoldersEp)

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
    , foldersEp :: as :- NamedRoutes VaultFoldersApi
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

newtype VaultFoldersApi as = VaultFoldersApi
    { getFoldersEp :: as :- "list" :> "object" :> "folders" :> Get '[JSON] (VaultResponse FoldersData)
    }
    deriving stock (Generic)

data VaultMiscellaneousApi as = VaultMiscellaneousApi
    { statusEp :: as :- "status" :> Get '[JSON] (VaultResponse StatusData)
    , syncEp :: as :- "sync" :> Post '[JSON] (VaultResponse TitledData)
    , generatePasswordEp ::
        as
            :- "generate"
                :> QueryParam "length" Int
                :> QueryFlag "uppercase"
                :> QueryFlag "lowercase"
                :> QueryFlag "number"
                :> QueryFlag "special"
                :> Get '[JSON] (VaultResponse GenerateData)
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
    parseJSON = withObject "ItemsData" $
        \o -> ItemsData <$> o .: "data"

data Item
    = ItemLogin Login
    | ItemCard Card
    | ItemSecureNote SecureNote
    | ItemIdentity Identity
    deriving stock (Show, Eq, Ord)

newtype GenerateData = GenerateData
    { unGenerateData :: Text
    }
    deriving stock (Show, Eq)

instance FromJSON GenerateData where
    parseJSON = withObject "GenerateData" $
        \o -> GenerateData <$> o .: "data"

toSymbol :: Item -> Text
toSymbol = \case
    ItemLogin _ -> "(l)"
    ItemCard _ -> "(c)"
    ItemIdentity _ -> "(i)"
    ItemSecureNote _ -> "(n)"

instance ToEntry ItemTemplate where
    toEntry (ItemTemplate _ _ name _ item) =
        mappend (toSymbol item <> " " <> name) $
            (\t -> if Text.null t then "" else " - " <> t) $
                merge $
                    case item of
                        ItemLogin itemLogin -> catMaybes [loginUsername itemLogin] <> maybe [] coerce (loginUris itemLogin)
                        ItemCard Card{..} -> [Text.take 5 cardNumber <> "..."]
                        ItemIdentity _ -> []
                        ItemSecureNote _ -> []

merge :: [Text] -> Text
merge = mconcat . intersperse " - "

instance FromJSON Folder where
    parseJSON = withObject "Folder" $ \o -> Folder <$> o .: "name"

newtype FoldersData = FoldersData
    { unFoldersData :: [Folder]
    }
    deriving stock (Show, Eq)

instance FromJSON FoldersData where
    parseJSON = withObject "FoldersData" $ \o -> FoldersData <$> o .: "data"
newtype Folder = Folder
    { unFolder :: Text
    }
    deriving (Show, Eq)

data ItemTemplate = ItemTemplate
    { itId       :: Text
    , itFolderId :: Maybe Text
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
    , loginTotp     :: Maybe Text
    }
    deriving stock (Show, Eq, Ord)

instance FromJSON Login where
    parseJSON = withObject "Login" $ \o ->
        Login <$> o .:? "uris" <*> o .:? "username" <*> o .: "password" <*> o .:? "totp"

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
        itId :: Text <- o .: "id"
        itType :: Int <- o .: "type"
        itFolderId <- o .: "folderId"
        itName <- o .: "name"
        itNotes <- o .: "notes"
        let item = ItemTemplate itId itFolderId itName itNotes
        case itType of
            1 -> item . ItemLogin <$> o .: "login"
            2 -> item . ItemSecureNote <$> o .: "secureNote"
            3 -> item . ItemCard <$> o .: "card"
            4 -> item . ItemIdentity <$> o .: "identity"
            _ -> fail "Invalid item type"
