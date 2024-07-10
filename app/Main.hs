{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DerivingStrategies   #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}

module Main where

import           Control.Monad              (replicateM_, void, when, (>=>))
import           Control.Monad.Trans.Class  (lift)
import           Control.Monad.Trans.Reader (ReaderT, asks, mapReaderT,
                                             runReaderT)
import           Data.Aeson                 (FromJSON (parseJSON),
                                             ToJSON (toJSON), decode, object,
                                             withObject, withText, (.:), (.:?),
                                             (.=))
import           Data.Bifunctor             (Bifunctor (first), bimap)
import           Data.Coerce                (coerce)
import           Data.Foldable              (for_)
import           Data.List                  (find, foldl', intersperse)
import           Data.Maybe                 (catMaybes, fromMaybe)
import           Data.Ord                   (comparing)
import           Data.Text                  (Text)
import qualified Data.Text                  as Text
import           Data.Time                  (UTCTime)
import           GHC.Generics               (Generic)
import           Network.HTTP.Client        (defaultManagerSettings, newManager)
import           Prelude                    hiding (log)
import           Servant                    (Capture, Get, JSON, NamedRoutes,
                                             Post, PostNoContent, Proxy (Proxy),
                                             Put, QueryFlag, QueryParam,
                                             ReqBody, (:-), (:>))
import           Servant.Client             (AsClientT, ClientEnv,
                                             ClientError (..), ClientM,
                                             Response, client, mkClientEnv,
                                             parseBaseUrl, responseBody,
                                             runClientM, (//))
import           Shelly                     (Sh, errorExit, exit, fromText,
                                             get_env, liftIO, readfile, run,
                                             run_, setStdin, shelly, silently,
                                             toTextIgnore, verbosely,
                                             withTmpDir, writefile, (<.>))
import           Text.Read                  (readMaybe)

default (Text)

class ToEntry a where
    toEntry :: a -> Text

paste :: Text -> Sh ()
paste text = xdotool ["type", "--delay", "0", text]

pressKey :: Text -> Sh ()
pressKey key = xdotool ["key", key]

xdotool :: [Text] -> Sh ()
xdotool = run_ "xdotool"

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

handleClientError :: ClientError -> Text
handleClientError clientError =
    case clientError of
        (DecodeFailure _df _)           -> "Decode failure"
        (ConnectionError _)             -> "Connection error"
        (UnsupportedContentType _ _res) -> "Unsupported content type"
        (InvalidContentTypeHeader _res) -> "Invalid content type header"
        (FailureResponse _ res)         -> reason res
  where
    reason :: Response -> Text
    reason =
        fromMaybe "Something went wrong"
            . coerce
            . decode @VaultFailureResponse
            . responseBody

askPassword :: Sh Password
askPassword = silently $ Password . head . Text.lines <$> run "dmenu" args
  where
    obscureColor = "#222222"
    args = ["-p", "Enter Password ", "-nb", obscureColor, "-nf", obscureColor]

login :: Password -> App UnlockData
login = callApi . (vaultClient // lockingEp // unlockEp)

logout :: App TitledData
logout = callApi (vaultClient // lockingEp // lockEp)

sync :: App TitledData
sync = callApi (vaultClient // miscEp // syncEp)

getItems :: App [ItemTemplate]
getItems = coerce <$> callApi (vaultClient // itemsEp // getItemsEp)

getStatus :: App StatusData
getStatus = callApi (vaultClient // miscEp // statusEp)

getFolders :: App [Folder]
getFolders = coerce <$> callApi (vaultClient // foldersEp // getFoldersEp)

callApiIO :: ClientM (VaultResponse a) -> ClientEnv -> IO (Either Text a)
callApiIO action env = bimap handleClientError unVaultResponse <$> runClientM action env

callApi :: ClientM (VaultResponse a) -> App a
callApi action = do
    env <- asks envClient
    liftIO (callApiIO action env) >>= \case
        Left e -> interactionLoop (announceI e) >> lift (exit 1)
        Right r -> pure r

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
    , editItemEp ::
        as
            :- "object"
                :> "item"
                :> Capture "id" Text
                :> ReqBody '[JSON] ItemTemplate
                :> Put
                    '[JSON]
                    (VaultResponse ItemTemplate)
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
    toEntry (ItemTemplate _ _ name _ _ item) =
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
    , itFavorite :: Bool
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

instance ToJSON Uri where
    toJSON (Uri uri) = object ["uri" .= uri]

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

instance ToJSON Login where
    toJSON Login{..} =
        object
            [ "uris" .= loginUris
            , "username" .= loginUsername
            , "password" .= loginPassword
            , "totp" .= loginTotp
            ]

data SecureNote = SecureNote
    deriving stock (Show, Eq, Ord)

instance FromJSON SecureNote where
    parseJSON = withObject "SecureNote" $ const (pure SecureNote)

instance ToJSON SecureNote where
    toJSON SecureNote = object []

data Card = Card
    { cardHolderName :: Text
    , cardNumber     :: Text
    , cardCode       :: Text
    , cardExpMonth   :: Text
    , cardExpYear    :: Text
    }
    deriving stock (Show, Eq, Ord)

instance FromJSON Card where
    parseJSON = withObject "Card" $ \o ->
        Card <$> o .: "cardholderName" <*> o .: "number" <*> o .: "code" <*> o .: "expMonth" <*> o .: "expYear"

instance ToJSON Card where
    toJSON Card{..} =
        object
            [ "cardholderName" .= cardHolderName
            , "number" .= cardNumber
            , "code" .= cardCode
            , "expMonth" .= cardExpMonth
            , "expYear" .= cardExpYear
            ]

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
        itFavorite <- o .: "favorite"
        let item = ItemTemplate itId itFolderId itName itNotes itFavorite
        case itType of
            1 -> item . ItemLogin <$> o .: "login"
            2 -> item . ItemSecureNote <$> o .: "secureNote"
            3 -> item . ItemCard <$> o .: "card"
            4 -> item . ItemIdentity <$> o .: "identity"
            _ -> fail "Invalid item type"

instance ToJSON ItemTemplate where
    toJSON ItemTemplate{..} =
        object
            [ "folderId" .= itFolderId
            , "name" .= itTitle
            , "notes" .= itNotes
            , "favorite" .= itFavorite
            , case itItem of
                ItemLogin l      -> "login" .= l
                ItemSecureNote _ -> "secureNote" .= ()
                ItemCard c       -> "card" .= c
                ItemIdentity _   -> "identity" .= ()
            ]

data Env = Env
    { envClient :: ClientEnv
    , envMenu   :: Menu
    }

newtype Menu = Menu
    { unMenu ::
        Prompt ->
        [Option] ->
        Sh (Either Text Option)
    }

type App = ReaderT Env Sh

data Arg = ArgObscured | ArgPrompt Text

newtype Prompt = Prompt {unPrompt :: [Arg]}

newtype Option = Option {unOption :: Text}
    deriving stock (Show, Eq, Ord)

data Interaction = InteractionQuestion Question | InteractionEnd

data Question = Question
    { questionPrompt         :: Prompt
    , questionOptions        :: [Option]
    , questionHandleResponse :: Either Text Option -> App Interaction
    }

typeItemI :: ItemTemplate -> Interaction
typeItemI item =
    InteractionQuestion $
        Question
            (Prompt [ArgPrompt "Entries"])
            (fst <$> typeItemOptions item)
            ( \case
                Left _ -> pure InteractionEnd
                Right i -> case i `lookup` typeItemOptions item of
                    Nothing -> pure InteractionEnd
                    Just i' -> i'
            )

typeItemOptions :: ItemTemplate -> [(Option, App Interaction)]
typeItemOptions (ItemTemplate{..}) =
    ( bimap Option (>> pure InteractionEnd)
        <$> [ ("Title: " <> itTitle, lift (paste itTitle))
            , ("Folder: " <> fromMaybe "None" itFolderId, lift (for_ itFolderId paste))
            ,
                ( "Notes: " <> maybe "None" (const "<Enter to view>") itNotes
                , lift (for_ itNotes openInEditor_)
                )
            ]
    )
        <> specificTypeItemOptions itItem

specificTypeItemOptions :: Item -> [(Option, App Interaction)]
specificTypeItemOptions item =
    bimap Option (>> pure InteractionEnd) <$> case item of
        ItemLogin Login{..} ->
            [ ("Username: " <> fromMaybe "None" loginUsername, lift (for_ loginUsername paste))
            , ("Password: ********", lift (for_ loginPassword paste))
            , ("TOTP: " <> fromMaybe "None" loginTotp, pure ())
            ]
                <> case fmap (zip [1 :: Int ..]) loginUris of
                    Just uris ->
                        [ ( "URL" <> Text.pack (show i) <> ": " <> coerce url
                          , lift (openInBrowser_ (coerce url))
                          )
                        | (i, url) <- uris
                        ]
                    Nothing -> []
        ItemCard Card{..} ->
            [ ("Cardholder Name: " <> cardHolderName, lift (paste cardHolderName))
            , ("Number: " <> cardNumber, lift (paste cardNumber))
            , ("Security Code: " <> cardCode, lift (paste cardCode))
            ]
        ItemIdentity _ -> []
        ItemSecureNote _ -> []

data EditType = Update | Create

editItemI :: EditType -> ItemTemplate -> Interaction
editItemI et item@(ItemTemplate{..}) =
    InteractionQuestion $
        Question
            (Prompt [ArgPrompt "Entries"])
            (fst <$> editItemOptions)
            ( \case
                Left _ -> pure InteractionEnd
                Right i -> case i `lookup` editItemOptions of
                    Nothing -> pure InteractionEnd
                    Just i' -> i'
            )
  where
    miscOptions :: [(Option, App Interaction)]
    miscOptions =
        [ (Option "Delete entry", pure InteractionEnd)
        ,
            ( Option "Save entry"
            , case et of
                Update -> callApi ((vaultClient // itemsEp // editItemEp) itId item) >> pure InteractionEnd
                Create -> callApi ((vaultClient // itemsEp // editItemEp) itId item) >> pure InteractionEnd
            )
        ]

    editItemOptions :: [(Option, App Interaction)]
    editItemOptions =
        ( first Option
            <$> [
                    ( "Title: " <> itTitle
                    , pure $
                        editI
                            "Title"
                            [itTitle]
                            ( \case
                                Right _ -> pure (editItemI et item)
                                Left new -> pure (editItemI et (item{itTitle = new}))
                            )
                    )
                ,
                    ( "Folder: " <> fromMaybe "None" itFolderId
                    , do
                        folders <- getFolders
                        pure $
                            editI
                                "Folder"
                                (coerce folders)
                                ( \case
                                    Right (Option new) -> pure (editItemI et (item{itFolderId = Just new}))
                                    Left _ -> pure (editItemI et item)
                                )
                    )
                ,
                    ( "Notes: " <> maybe "None" (const "<Enter to edit>") itNotes
                    , do
                        newNotes <- lift (openInEditor (fromMaybe "" itNotes))
                        pure $ editItemI et (item{itNotes = Just newNotes})
                    )
                ]
        )
            <> specificEditItemOptions
            <> miscOptions

    specificEditItemOptions :: [(Option, App Interaction)]
    specificEditItemOptions =
        first Option <$> case itItem of
            (ItemLogin l@Login{..}) ->
                [
                    ( "Username: " <> fromMaybe "None" loginUsername
                    , pure $
                        editI
                            "Username"
                            [fromMaybe "" loginUsername]
                            ( \case
                                Right _ -> pure (editItemI et item)
                                Left new ->
                                    pure
                                        ( editItemI
                                            et
                                            (item{itItem = ItemLogin l{loginUsername = Just new}})
                                        )
                            )
                    )
                ,
                    ( "Password: ********"
                    , do
                        mpw <- generatePassword
                        case mpw of
                            Just pw ->
                                pure
                                    ( editItemI
                                        et
                                        (item{itItem = ItemLogin l{loginPassword = Just pw}})
                                    )
                            Nothing -> pure (editItemI et item)
                    )
                ]
            --                <> case fmap (zip [1 :: Int ..]) loginUris of
            --                    Just uris ->
            --                        [ ("URL" <> Text.pack (show i) <> ": " <> coerce url, lift (openInBrowser_ (coerce url)))
            --                        | (i, url) <- uris
            --                        ]
            --                    Nothing -> []
            ItemCard c@Card{..} ->
                [
                    ( "Cardholder Name: " <> cardHolderName
                    , pure $
                        editI
                            "Cardholder Name"
                            [cardHolderName]
                            ( \case
                                Right _ -> pure (editItemI et item)
                                Left new ->
                                    pure
                                        ( editItemI
                                            et
                                            (item{itItem = ItemCard c{cardHolderName = new}})
                                        )
                            )
                    )
                ,
                    ( "Number: " <> cardNumber
                    , pure $
                        editI
                            "Number"
                            [cardNumber]
                            ( \case
                                Right _ ->
                                    pure
                                        (editItemI et item)
                                Left new ->
                                    pure
                                        ( editItemI
                                            et
                                            (item{itItem = ItemCard c{cardNumber = new}})
                                        )
                            )
                    )
                ,
                    ( "Security Code: " <> cardCode
                    , pure $
                        editI
                            "Security Code"
                            [cardCode]
                            ( \case
                                Right _ -> pure (editItemI et item)
                                Left new -> pure (editItemI et (item{itItem = ItemCard c{cardCode = new}}))
                            )
                    )
                ]
            ItemIdentity _ -> []
            ItemSecureNote _ -> []

editI :: Text -> [Text] -> (Either Text Option -> App Interaction) -> Interaction
editI prompt olds next =
    InteractionQuestion $
        Question (Prompt [ArgPrompt prompt]) (coerce olds) next

loginI :: Interaction
loginI =
    InteractionQuestion $
        Question
            (Prompt [ArgPrompt "Enter Password", ArgObscured])
            []
            ( \case
                Right _ -> pure InteractionEnd
                Left pw -> login (Password pw) >> pure InteractionEnd
            )

dashboardI :: [ItemTemplate] -> Interaction
dashboardI items =
    InteractionQuestion $
        Question
            (Prompt [ArgPrompt "Entries"])
            (fst <$> continuations)
            ( \case
                Left _ -> pure InteractionEnd
                Right i -> case i `lookup` continuations of
                    Nothing -> pure InteractionEnd
                    Just i' -> i'
            )
  where
    continuations :: [(Option, App Interaction)]
    continuations =
        coerce $
            [
                ( "View/type individual items"
                , allItemsI typeItemI <$> getItems
                )
            ,
                ( "View previous entry"
                , do
                    itemId <- lift readCache
                    let cachedItem = find ((== itemId) . itId) items
                    pure $ case cachedItem of
                        Just item -> typeItemI item
                        Nothing   -> announceI "No previous entry"
                )
            , ("Edit entries", allItemsI (editItemI Update) <$> getItems)
            , ("Add entry", pure addNewItemI)
            , ("Manage folders", pure InteractionEnd)
            , ("Manage collections", pure InteractionEnd)
            , ("Sync vault", pure InteractionEnd)
            , ("Switch vaults", pure InteractionEnd)
            , ("Lock vault", logout >> pure InteractionEnd)
            , ("-------- Quick actions --------", pure InteractionEnd)
            ]
                <> map (\item -> (toEntry item, quickAction item)) items

    quickAction :: ItemTemplate -> App Interaction
    quickAction i =
        case itItem i of
            ItemIdentity _ -> lift (paste "identity")
            ItemSecureNote _ -> case itNotes i of
                Just n  -> lift (openInEditor_ n)
                Nothing -> interactionLoop (announceI "No note")
            ItemCard (Card{..}) ->
                lift (openInEditor_ (Text.unlines [cardHolderName, cardNumber, cardCode]))
            ItemLogin (Login{..}) -> case (loginUsername, loginPassword) of
                (Just u, Just p) -> lift $ do
                    paste u
                    pressKey "Tab"
                    paste p
                    replicateM_ 2 (pressKey "Return")
                _ -> announce "No username or password"
            >> pure InteractionEnd

addNewItemI :: Interaction
addNewItemI =
    InteractionQuestion $
        Question
            (Prompt [ArgPrompt "Item Type"])
            (fst <$> options)
            ( \case
                Left _ -> pure InteractionEnd
                Right o -> case o `lookup` options of
                    Nothing -> pure InteractionEnd
                    Just i  -> i
            )
  where
    options :: [(Option, App Interaction)]
    options =
        first Option
            <$> [
                    ( "Login"
                    , editItemI Create . emptyItem <$> pickFolder
                    )
                , ("Secure Note", pure InteractionEnd)
                , ("Card", pure InteractionEnd)
                , ("Identity", pure InteractionEnd)
                ]

    emptyItem :: Folder -> ItemTemplate
    emptyItem folder =
        ItemTemplate
            { itId = ""
            , itFolderId = Just (unFolder folder)
            , itTitle = ""
            , itNotes = Nothing
            , itFavorite = False
            , itItem =
                ItemLogin
                    ( Login
                        { loginUris = Nothing
                        , loginUsername = Nothing
                        , loginPassword = Nothing
                        , loginTotp = Nothing
                        }
                    )
            }

    pickFolder :: App Folder
    pickFolder = do
        Menu menu <- asks envMenu
        folders <- getFolders
        Right (Option f) <- lift $ menu (Prompt [ArgPrompt "Folder"]) (coerce folders)
        return (Folder f)

addNewItemI' :: Interaction
addNewItemI' = InteractionQuestion $ Question (Prompt []) [] (const $ pure InteractionEnd)

announceI :: Text -> Interaction
announceI a =
    InteractionQuestion $
        Question
            (Prompt [ArgPrompt a])
            []
            (const $ pure InteractionEnd)

announce :: Text -> App ()
announce = interactionLoop . announceI

allItemsI :: (ItemTemplate -> Interaction) -> [ItemTemplate] -> Interaction
allItemsI interaction items =
    InteractionQuestion $
        Question
            (Prompt [ArgPrompt "Entries"])
            (fst <$> continuations)
            ( \case
                Left _ -> pure InteractionEnd
                Right o -> case o `lookup` continuations of
                    Nothing -> pure InteractionEnd
                    Just i  -> i
            )
  where
    continuations :: [(Option, App Interaction)]
    continuations = (\t -> (Option (toEntry t), lift (writeCache (itId t)) >> pure (interaction t))) <$> items

main :: IO ()
main = do
    manager <- newManager defaultManagerSettings
    baseUrl <- parseBaseUrl "http://localhost:8087"
    let env = mkClientEnv manager baseUrl

    shelly $ verbosely $ runReaderT app (Env env (Menu dmenu))

app :: App ()
app = do
    status <- getStatus

    when
        (statusDataStatus status == Locked)
        (mapReaderT silently (interactionLoop loginI))

    items <- getItems

    interactionLoop (dashboardI items)

interactionLoop :: Interaction -> App ()
interactionLoop InteractionEnd = pure ()
interactionLoop (InteractionQuestion q) = do
    response <- askQuestion q
    interaction <- questionHandleResponse q response
    interactionLoop interaction

askQuestion :: Question -> App (Either Text Option)
askQuestion q = do
    Menu menu <- asks envMenu
    lift (menu (questionPrompt q) (questionOptions q))

dmenu :: Prompt -> [Option] -> Sh (Either Text Option)
dmenu prompt options = do
    let ls = Text.unlines (coerce options)
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
    pure $
        if line `elem` map coerce options
            then Right (Option line)
            else Left line

openInEditor_ :: Text -> Sh ()
openInEditor_ = void . openInEditor

openInBrowser_ :: Text -> Sh ()
openInBrowser_ = void . openInBrowser

openInBrowser :: Text -> Sh Text
openInBrowser = run "firefox" . pure

openInEditor :: Text -> Sh Text
openInEditor text = do
    editor <- get_env "EDITOR"
    case editor of
        Nothing -> errorExit "EDITOR not set"
        Just e -> withTmpDir $ \fp -> do
            writefile (fp <.> "hwarden") text
            run_ "st" [e, toTextIgnore (fp <.> "hwarden")]
            readfile (fp <.> "hwarden")

generatePassword :: App (Maybe Text)
generatePassword = do
    Menu menu <- asks envMenu
    Right (Option m) <- lift $ menu (Prompt [ArgPrompt "Method"]) [Option "Generate", Option "Manual"]
    case m of
        "Manual" ->
            lift $
                menu (Prompt [ArgPrompt "Password"]) [] >>= \case
                    Left newPw -> pure (Just newPw)
                    Right _ -> pure Nothing
        "Generate" -> do
            len <- lift $ either id coerce <$> menu (Prompt [ArgPrompt "Length"]) [Option "20"]
            Right (Option s) <- lift $ menu (Prompt [ArgPrompt "Include special?"]) [Option "Yes", Option "No"]
            case readMaybe @Int (Text.unpack len) of
                Nothing -> pure Nothing
                Just len' -> do
                    let gen spec = Just . unGenerateData <$> callApi ((vaultClient // miscEp // generatePasswordEp) (Just len') False False False spec)
                    case s of
                        "Yes" -> gen True
                        "No"  -> gen False
                        _     -> pure Nothing
        _ -> undefined
