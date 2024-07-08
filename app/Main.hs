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
import           Data.Bifunctor             (bimap, first)
import           Data.Coerce                (coerce)
import           Data.List                  (find, foldl', intersperse)
import qualified Data.Map                   as Map
import           Data.Maybe                 (catMaybes, fromMaybe)
import           Data.Ord                   (comparing)
import           Data.Text                  (Text)
import qualified Data.Text                  as Text
import           Data.Time                  (UTCTime)
import           GHC.Generics               (Generic)
import           Network.HTTP.Client        (defaultManagerSettings, newManager)
import           Prelude                    hiding (log)
import           Servant                    (Get, JSON, NamedRoutes, Post,
                                             PostNoContent, Proxy (Proxy),
                                             QueryFlag, QueryParam, ReqBody,
                                             (:-), (:>))
import           Servant.Client             (AsClientT, ClientEnv,
                                             ClientError (..), ClientM,
                                             Response, client, mkClientEnv,
                                             parseBaseUrl, responseBody,
                                             runClientM, (//))
import           Shelly                     (Sh, errorExit, exit, fromText,
                                             get_env, liftIO, readfile, run,
                                             run_, setStdin, shelly, silently,
                                             toTextIgnore, withTmpDir,
                                             writefile, (<.>))

-- generatePassword :: ClientEnv -> Sh Text
-- generatePassword env = do
--     method <- dmenuSelect [] "Method" (Text.unlines ["Generate", "Manual"])
--     case method of
--         "Generate" -> do
--             len <- dmenuSelect [] "Length" "20"
--             special <- dmenuSelect [] "Include special?" (Text.unlines ["Yes", "No"])
--             case readMaybe @Int (Text.unpack len) of
--                 Nothing -> showExit "Invalid length"
--                 Just len' -> do
--                     let gen spec = callApi (generatePassword' (Just len') False False False spec) env <&> unGenerateData
--                     case head (Text.lines special) of
--                         "Yes" -> gen True
--                         "No"  -> gen False
--                         _     -> showExit "Please choose Yes or No"
--         "Manual" -> dmenuSelect [] "Password" ""
--         _ -> showExit "Please choose Generate or Manual"
--   where
--     generatePassword' :: Maybe Int -> Bool -> Bool -> Bool -> Bool -> ClientM (VaultResponse GenerateData)
--     generatePassword' = vaultClient // miscEp // generatePasswordEp

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

getFolders :: App FoldersData
getFolders = callApi (vaultClient // foldersEp // getFoldersEp)

callApiIO :: ClientM (VaultResponse a) -> ClientEnv -> IO (Either Text a)
callApiIO action env = bimap handleClientError unVaultResponse <$> runClientM action env

callApi :: ClientM (VaultResponse a) -> App a
callApi action = do
    env <- asks envClient
    liftIO (callApiIO action env) >>= \case
        Left e -> interactionLoop (announceInteraction e) >> lift (exit 1)
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

loginInteraction :: Interaction
loginInteraction =
    InteractionQuestion $
        Question
            (Prompt [ArgPrompt "Enter Password", ArgObscured])
            []
            ( \case
                Right _ -> undefined
                Left pw -> login (Password pw) >> pure InteractionEnd
            )

dashboardInteraction :: [ItemTemplate] -> Interaction
dashboardInteraction items =
    InteractionQuestion $
        Question
            (Prompt [ArgPrompt "Entries"])
            (fst <$> continuations)
            ( \case
                Left _ -> pure InteractionEnd
                Right i -> case Map.lookup i (Map.fromList continuations) of
                    Nothing -> pure InteractionEnd
                    Just i' -> i'
            )
  where
    continuations :: [(Option, App Interaction)]
    continuations =
        coerce $
            [
                ( "View/type individual items"
                , allItemsInteraction <$> getItems
                )
            , ("View previous item", pure InteractionEnd)
            , ("Edit entries", pure InteractionEnd)
            , ("Add entry", pure InteractionEnd)
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
        pure InteractionEnd
            << case itItem i of
                ItemIdentity _ -> lift (paste "identity")
                ItemSecureNote _ -> case itNotes i of
                    Just n  -> lift (openInEditor_ n)
                    Nothing -> interactionLoop (announceInteraction "No note")
                ItemCard (Card{..}) ->
                    lift (openInEditor_ (Text.unlines [cardHolderName, cardNumber, cardCode]))
                ItemLogin (Login{..}) -> case (loginUsername, loginPassword) of
                    (Just u, Just p) -> lift $ do
                        paste u
                        pressKey "Tab"
                        paste p
                        replicateM_ 2 (pressKey "Return")
                    _ -> interactionLoop (announceInteraction "No username or password")

(<<) :: (Monad m) => m a -> m b -> m a
(<<) = flip (>>)

announceInteraction :: Text -> Interaction
announceInteraction a =
    InteractionQuestion $
        Question
            (Prompt [ArgPrompt a])
            []
            (const $ pure InteractionEnd)

allItemsInteraction :: [ItemTemplate] -> Interaction
allItemsInteraction items =
    InteractionQuestion $
        Question
            (Prompt [ArgPrompt "Entries"])
            (fst <$> continuations)
            ( \case
                Left _ -> pure InteractionEnd
                Right o -> case continuations `findContinuation` o of
                    Nothing -> pure InteractionEnd
                    Just i  -> i
            )
  where
    continuations :: [(Option, App Interaction)]
    continuations = map ((,pure InteractionEnd) . Option . toEntry) items

findContinuation :: (Foldable t, Eq a) => t (a, b) -> a -> Maybe b
findContinuation conts o = snd <$> find ((== o) . fst) conts

main :: IO ()
main = do
    manager <- newManager defaultManagerSettings
    baseUrl <- parseBaseUrl "http://localhost:8087"
    let env = mkClientEnv manager baseUrl

    shelly $ runReaderT app (Env env (Menu dmenu))

app :: App ()
app = do
    status <- getStatus

    when
        (statusDataStatus status == Locked)
        (mapReaderT silently (interactionLoop loginInteraction))

    items <- getItems

    interactionLoop (dashboardInteraction items)

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

openInEditor :: Text -> Sh Text
openInEditor text = do
    editor <- get_env "EDITOR"
    case editor of
        Nothing -> errorExit "EDITOR not set"
        Just e -> withTmpDir $ \fp -> do
            writefile (fp <.> "hwarden") text
            run_ "st" [e, toTextIgnore (fp <.> "hwarden")]
            readfile (fp <.> "hwarden")
