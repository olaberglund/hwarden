{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DerivingStrategies   #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}

module Main where

import           Control.Arrow       (first)
import           Control.Monad       (replicateM_, void, when, (>=>))
import           Data.Aeson          (FromJSON (parseJSON), ToJSON (toJSON),
                                      decode, object, withObject, withText,
                                      (.:), (.:?), (.=))
import           Data.Coerce         (coerce)
import           Data.Functor        ((<&>))
import           Data.List           (find, foldl', intersperse)
import           Data.Map            (Map)
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
import           Shelly              (Sh, errorExit, exit, fromText, get_env,
                                      liftIO, readfile, run, run_, setStdin,
                                      shelly, silently, toTextIgnore,
                                      withTmpDir, writefile, (<.>))
import           Text.Read           (readMaybe)

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

individualItemEntries :: ItemTemplate -> [Entry ()]
individualItemEntries it =
    commonEntries <> case itItem it of
        ItemLogin l         -> loginEntries l
        ItemCard c          -> cardEntries c
        ItemSecureNote _    -> noteEntries it
        ItemIdentity _ident -> []
  where
    commonEntries =
        [ Entry ("Name:  " <> itTitle it) (pure ())
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
        [ Entry
            ("Name: " <> itTitle it)
            ( do
                name <- dmenuSelect [] "Name" (itTitle it)
                handleEditItemView env it{itTitle = name}
            )
        , Entry
            ("Folder: " <> orNone (itFolderId it))
            ( do
                folders <- getFolders env
                folderId <- dmenuSelect [] "Folder" (Text.unlines (coerce folders))
                handleEditItemView env it{itFolderId = Just folderId}
            )
        , Entry
            ("Notes: " <> maybe "<Enter to add>" (const "<Enter to edit>") (itNotes it))
            ( do
                note <- openInEditor (fromMaybe "" (itNotes it))
                handleEditItemView env it{itNotes = Just note}
            )
        , Entry "Delete entry" (dmenuShow "TODO")
        , Entry "Save entry" (dmenuShow "TODO")
        ]

    orNone :: Maybe Text -> Text
    orNone = fromMaybe "None"

    loginEntries :: Login -> [Entry ()]
    loginEntries l@(Login uris name _pw totp) =
        [ Entry
            ("Username: " <> orNone name)
            ( do
                un <- dmenuSelect [] "Username" (orNone name)
                handleEditItemView env it{itItem = ItemLogin l{loginUsername = Just un}}
            )
        , Entry
            "Password: ********"
            ( do
                pw <- generatePassword env
                handleEditItemView env it{itItem = ItemLogin l{loginPassword = Just pw}}
            )
        , Entry
            "URLs: <Enter to edit>"
            ( do
                url <- dmenuSelect [] "URL (type to add new)" (Text.unlines (maybe [] coerce uris))
                case find ((== url) . unUri) (fromMaybe [] uris) of
                    Just oldUrl -> do
                        newUrl <- dmenuSelect [] "URL" (coerce oldUrl)
                        handleEditItemView
                            env
                            it{itItem = ItemLogin (l{loginUris = Just (Uri newUrl : filter (/= oldUrl) (fromMaybe [] uris))})}
                    Nothing ->
                        handleEditItemView
                            env
                            it{itItem = ItemLogin (l{loginUris = Just (Uri url : fromMaybe [] uris)})}
            )
        , Entry ("TOTP: " <> orNone totp) (dmenuShow "TODO")
        ]

handleDashboardView :: ClientEnv -> [ItemTemplate] -> Sh ()
handleDashboardView env items = handleView "Entries" allEntries
  where
    miscEntries :: [Entry ()]
    miscEntries =
        [ Entry "View/Type individual entries" (handleAllItemsView items)
        , Entry "View previous entry" (readCache >>= itemWithId items >>= handleItemView)
        , Entry "Edit entry (WIP)" (handleEditItemsView env items)
        , Entry "Add entry (TODO)" (pure ())
        , Entry "Manage folders (TODO)" (pure ())
        , Entry "Manage collections (TODO)" (pure ())
        , Entry "Switch vaults (TODO)" (pure ())
        , Entry "Sync vault" (sync env)
        , Entry "Lock vault" (logout env)
        ]

    allEntries = miscEntries <> itemEntries items dashboardAction

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
handleAllItemsView items = handleView "Entries" entries
  where
    entries = itemEntries items (\i -> writeCache (itId i) >> handleItemView i)

handleEditItemsView :: ClientEnv -> [ItemTemplate] -> Sh ()
handleEditItemsView env items = handleView "Entries" entries
  where
    entries = itemEntries items (handleEditItemView env)

handleEditItemView :: ClientEnv -> ItemTemplate -> Sh ()
handleEditItemView env item = handleView "Entry" entries
  where
    entries = individualEditItemEntries env item

handleItemView :: ItemTemplate -> Sh ()
handleItemView item = handleView "Entry" (individualItemEntries item)

handleView :: forall a. Text -> [Entry a] -> Sh a
handleView prompt entries =
    dmenuSelect [] prompt (Text.unlines (map entryLabel entries))
        >>= runSelected
  where
    runSelected :: Text -> Sh a
    runSelected selected = case Map.lookup selected (entriesToMap entries) of
        Just action -> action
        Nothing     -> showExit ("No such entry: " <> selected)

    entriesToMap :: [Entry a] -> Map.Map Text (Sh a)
    entriesToMap = Map.fromList . map (\e -> (entryLabel e, entryRun e))

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
logout env = callApi lock env >>= dmenuShow . coerce
  where
    lock = vaultClient // lockingEp // lockEp

sync :: ClientEnv -> Sh ()
sync env = callApi sync' env >>= dmenuShow . coerce
  where
    sync' = vaultClient // miscEp // syncEp

generatePassword :: ClientEnv -> Sh Text
generatePassword env = do
    method <- dmenuSelect [] "Method" (Text.unlines ["Generate", "Manual"])
    case method of
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
    , itTitle    :: Text
    , itNotes    :: Maybe Text
    , itItem     :: Item
    }
    deriving (Show, Eq)

instance Ord ItemTemplate where
    compare = comparing itItem <> comparing itTitle

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

------------------------------------
------------------------------------
------------------------------------
------------------------------------
------------------------------------
------------------------------------
------------------------------------
------------------------------------
------------------------------------
------------------------------------
------------------------------------
------------------------------------
------------------------------------
------------------------------------
------------------------------------
------------------------------------
------------------------------------
------------------------------------
------------------------------------
------------------------------------
------------------------------------
------------------------------------
------------------------------------
------------------------------------
------------------------------------
------------------------------------
------------------------------------
------------------------------------
------------------------------------
------------------------------------
------------------------------------
------------------------------------
------------------------------------
------------------------------------
------------------------------------
------------------------------------
------------------------------------
------------------------------------
------------------------------------
------------------------------------
------------------------------------
------------------------------------
------------------------------------
------------------------------------
------------------------------------
------------------------------------
------------------------------------
------------------------------------
------------------------------------
------------------------------------
------------------------------------
------------------------------------
------------------------------------
------------------------------------
------------------------------------
------------------------------------
------------------------------------
------------------------------------
------------------------------------
------------------------------------
------------------------------------
------------------------------------
------------------------------------
------------------------------------
------------------------------------
------------------------------------
------------------------------------
------------------------------------
------------------------------------
------------------------------------
------------------------------------
------------------------------------
------------------------------------
------------------------------------
------------------------------------
------------------------------------
------------------------------------
------------------------------------
------------------------------------
------------------------------------
------------------------------------
------------------------------------
------------------------------------
------------------------------------
------------------------------------
------------------------------------
------------------------------------
------------------------------------
------------------------------------
------------------------------------
------------------------------------
------------------------------------
------------------------------------
------------------------------------
------------------------------------
------------------------------------
------------------------------------
------------------------------------
------------------------------------
------------------------------------
------------------------------------
------------------------------------
------------------------------------
------------------------------------
------------------------------------
------------------------------------
------------------------------------
------------------------------------
------------------------------------
------------------------------------
------------------------------------
------------------------------------
------------------------------------

data Arg = ArgObscured | ArgPrompt Text

newtype Error = Error Text

data Interaction
    = InteractionQuestion Question
    | InteractionEnd

newtype Prompt = Prompt {unPrompt :: [Arg]}

data Question = Question
    { questionPrompt        :: !Prompt
    , questionContinuations :: !(Map Option Interaction)
    , questionContext       :: !Context
    }

data Context
    = ContextLogin
    | ContextDashboard
    | ContextAllItems
    | ContextAnnouncement
    | ContextViewIndividualItem

data Option
    = OptionDashboard Dashboard
    | OptionIndividualItem IndividualItem
    | OptionFreeHand Text
    | OptionAllItems ItemTemplate
    deriving (Show, Eq, Ord)

data IndividualItem
    = IndividualItemPasteTitle Text
    | IndividualItemOpenEditorNotes (Maybe Text)
    | IndividualItemDoNothingFolder (Maybe Text)
    deriving (Show, Eq, Ord)

data Dashboard
    = DashboardViewAll
    | DashboardSync
    | DashboardLockVault
    | DashboardEntry ItemTemplate
    deriving (Show, Eq, Ord)

toText :: Option -> Text
toText (OptionFreeHand _) = ""
toText (OptionDashboard d) = case d of
    DashboardViewAll   -> "View/Type individual entries"
    DashboardSync      -> "Sync vault"
    DashboardLockVault -> "Lock vault"
    DashboardEntry it  -> toEntry it
toText (OptionIndividualItem action) = case action of
    IndividualItemDoNothingFolder t -> "Folder: " <> maybe "None" (head . Text.lines) t
    IndividualItemOpenEditorNotes t -> "Notes: " <> maybe "None" (head . Text.lines) t
    IndividualItemPasteTitle t -> "Title: " <> t
toText (OptionAllItems it) = toEntry it

viewItemI :: ItemTemplate -> Interaction
viewItemI item =
    InteractionQuestion
        ( Question
            { questionPrompt = Prompt [ArgPrompt "Entry"]
            , questionContinuations = Map.fromList (itemContinuations item)
            , questionContext = ContextViewIndividualItem
            }
        )

allItemsI :: [ItemTemplate] -> Interaction
allItemsI items =
    InteractionQuestion
        ( Question
            { questionPrompt = Prompt [ArgPrompt "Entries"]
            , questionContinuations =
                Map.fromList $ map (\it -> (OptionAllItems it, viewItemI it)) items
            , questionContext = ContextAllItems
            }
        )

itemContinuations :: ItemTemplate -> [(Option, Interaction)]
itemContinuations item =
    first OptionIndividualItem
        <$> map
            (,InteractionEnd)
            [ IndividualItemPasteTitle (itTitle item)
            , IndividualItemOpenEditorNotes (itNotes item)
            , IndividualItemDoNothingFolder (itFolderId item)
            ]

loginI :: Interaction
loginI =
    InteractionQuestion
        ( Question
            { questionPrompt = Prompt [ArgPrompt "Enter Password", ArgObscured]
            , questionContinuations = Map.empty
            , questionContext = ContextLogin
            }
        )

dashboardI :: [ItemTemplate] -> Interaction
dashboardI items =
    InteractionQuestion
        ( Question
            { questionPrompt = Prompt [ArgPrompt "Entries"]
            , questionContinuations =
                Map.fromList $
                    first OptionDashboard
                        <$> [ (DashboardViewAll, allItemsI items)
                            , (DashboardSync, InteractionEnd)
                            , (DashboardLockVault, InteractionEnd)
                            ]
                            <> map ((,InteractionEnd) . DashboardEntry) items
            , questionContext = ContextDashboard
            }
        )

announceI :: Text -> Interaction
announceI msg =
    InteractionQuestion
        ( Question
            (Prompt [ArgPrompt msg])
            Map.empty
            ContextAnnouncement
        )

main2 :: IO ()
main2 = do
    manager <- newManager defaultManagerSettings
    baseUrl <- parseBaseUrl "http://localhost:8087"
    let env = mkClientEnv manager baseUrl
    let menu = Menu dmenu
    shelly $ do
        status <- getStatus env

        when (statusDataStatus status == Locked) $
            silently $
                runInteraction menu env loginI

        items <- getItems env

        runInteraction menu env (dashboardI items)

runInteraction :: Menu -> ClientEnv -> Interaction -> Sh ()
runInteraction _ _ InteractionEnd = exit 0
runInteraction menu env (InteractionQuestion q) = do
    sel <- coerce menu q

    let continue = runInteraction menu env (questionContinuations q Map.! sel)

    case sel of
        OptionDashboard d -> case d of
            DashboardSync       -> sync env
            DashboardLockVault  -> logout env
            DashboardEntry item -> dashboardAction item
            _                   -> continue
        OptionFreeHand t -> case questionContext q of
            ContextLogin -> void (login env (Password t))
            ContextAnnouncement -> pure ()
            _ -> runInteraction menu env (announceI "No such option")
        _ -> continue

getItems' :: ClientEnv -> IO [ItemTemplate]
getItems' = coerce . callApi' (vaultClient // itemsEp // getItemsEp)

callApi' :: ClientM (VaultResponse a) -> ClientEnv -> IO a
callApi' action env =
    liftIO (runClientM action env) >>= \case
        Left e -> print e >> undefined
        Right (VaultResponse res) -> pure res

newtype Menu = Menu {unMenu :: Question -> Sh Option}

dmenu :: Question -> Sh Option
dmenu (Question prompt options _context) = do
    let textOptions = toText <$> Map.keys options
    let ls = Text.unlines textOptions
    let optionMap = Map.fromList (zip textOptions (Map.keys options))
    let lenLines = Text.pack (show (min 24 (length (Text.lines ls))))
    let obscureColor = "#222222"

    let fromArg current arg =
            current <> case arg of
                ArgObscured -> ["-nb", obscureColor, "-nf", obscureColor]
                ArgPrompt p -> ["-p", p]

    let args = foldl' fromArg ["-i", "-l", lenLines] (unPrompt prompt)

    setStdin ls
    sel <- run "dmenu" args
    let line = head (Text.lines sel)
    pure $ case Map.lookup line optionMap of
        Just o  -> o
        Nothing -> OptionFreeHand line

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
