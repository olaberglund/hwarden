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
import           Data.Coerce                (Coercible, coerce)
import           Data.Foldable              (for_)
import           Data.List                  (find, foldl', intersperse)
import           Data.Maybe                 (catMaybes, fromMaybe)
import           Data.Ord                   (comparing)
import           Data.Text                  (Text)
import qualified Data.Text                  as Text
import           Data.Time                  (UTCTime)
import           Debug.Trace                (traceShow, traceShowId, traceShowM)
import           GHC.Generics               (Generic)
import           Network.HTTP.Client        (defaultManagerSettings, newManager)
import           Prelude                    hiding (log)
import           Servant                    (Capture, Delete, Get, JSON,
                                             NamedRoutes, Post, PostNoContent,
                                             Proxy (Proxy), Put, QueryFlag,
                                             QueryParam, ReqBody, (:-), (:>))
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
        (DecodeFailure df _)            -> traceShow df "Decode failure"
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
            . traceShowId

login :: Password -> App UnlockData
login = mapReaderT (fmap coerce) . callApi . (vaultClient // lockingEp // unlockEp)

logout :: App TitledData
logout = mapReaderT (fmap coerce) $ callApi (vaultClient // lockingEp // lockEp)

sync :: App TitledData
sync = mapReaderT (fmap coerce) $ callApi (vaultClient // miscEp // syncEp)

getItems :: App [ItemTemplate]
getItems = mapReaderT (fmap coerce) $ callApi (vaultClient // itemsEp // getItemsEp)

getStatus :: App StatusData
getStatus = mapReaderT (fmap coerce) $ callApi (vaultClient // miscEp // statusEp)

getFolders :: App [Folder]
getFolders = mapReaderT (fmap coerce) $ callApi (vaultClient // foldersEp // getFoldersEp)

callApiIO :: ClientM a -> ClientEnv -> IO (Either Text a)
callApiIO action env = first handleClientError <$> runClientM action env

callApi :: ClientM a -> ReaderT Env Sh a
callApi action = do
    env <- asks envClient
    liftIO (callApiIO action env) >>= \case
        Left e -> announce e >> lift (exit 1)
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

data EmptyVaultResponse = EmptyVaultResponse
    deriving stock (Show, Eq)

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

instance FromJSON EmptyVaultResponse where
    parseJSON = withObject "VaultResponse" $ \o -> do
        success <- o .: "success"
        if success
            then pure EmptyVaultResponse
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
    { addItemEp :: as :- "object" :> "item" :> ReqBody '[JSON] ItemTemplate :> Post '[JSON] EmptyVaultResponse
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
    , deleteItemEp :: as :- "object" :> "item" :> Capture "id" Text :> Delete '[JSON] EmptyVaultResponse
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
    toEntry ItemTemplate{..} =
        mappend (toSymbol itItem <> " " <> itName) $
            (\t -> if Text.null t then "" else " - " <> t) $
                merge $
                    case itItem of
                        ItemLogin itemLogin -> catMaybes [loginUsername itemLogin] <> maybe [] (map unUri) (loginUris itemLogin)
                        ItemCard Card{..} -> catMaybes [(<> "...") . Text.take 5 <$> cardNumber]
                        ItemIdentity Identity{..} -> catMaybes [identityTitle, identitySsn]
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
    { itId            :: Text
    , itOrgId         :: Maybe Text
    , itCollectionIds :: [Text]
    , itType          :: Int
    , itFolderId      :: Maybe Text
    , itName          :: Text
    , itFields        :: Maybe [Field]
    , itNotes         :: Maybe Text
    , itRePrompt      :: Bool
    , itFavorite      :: Bool
    , itItem          :: Item
    }
    deriving (Show, Eq)

instance Ord ItemTemplate where
    compare = comparing itItem <> comparing itName

data Field = Field
    { fieldName  :: Text
    , fieldValue :: Text
    , fieldType  :: Int
    }
    deriving (Show, Eq)

instance FromJSON Field where
    parseJSON = withObject "Field" $ \o ->
        Field <$> o .: "name" <*> o .: "value" <*> o .: "type"

instance ToJSON Field where
    toJSON (Field{..}) =
        object
            [ "name" .= fieldName
            , "value" .= fieldValue
            , "type" .= fieldType
            ]

data Uri = Uri
    { unUri    :: Text
    , uriMatch :: Maybe Int
    }
    deriving stock (Show, Eq, Ord)

instance FromJSON Uri where
    parseJSON = withObject "Uri" $ \o ->
        Uri <$> o .: "uri" <*> o .: "match"

instance ToJSON Uri where
    toJSON (Uri uri match) = object ["uri" .= uri, "match" .= match]

data Login = Login
    { loginUris     :: Maybe [Uri]
    , loginUsername :: Maybe Text
    , loginPassword :: Maybe Text
    , loginTotp     :: Maybe Text
    }
    deriving stock (Show, Eq, Ord)

instance FromJSON Login where
    parseJSON = withObject "Login" $ \o ->
        Login
            <$> o .:? "uris"
            <*> o .:? "username"
            <*> o .: "password"
            <*> o .:? "totp"

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
    { cardHolderName :: Maybe Text
    , cardNumber     :: Maybe Text
    , cardCode       :: Maybe Text
    , cardExpMonth   :: Maybe Text
    , cardExpYear    :: Maybe Text
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
    { identityTitle          :: Maybe Text
    , identityFirstName      :: Maybe Text
    , identityMiddleName     :: Maybe Text
    , identityLastName       :: Maybe Text
    , identityAddress1       :: Maybe Text
    , identityAddress2       :: Maybe Text
    , identityAddress3       :: Maybe Text
    , identityCity           :: Maybe Text
    , identityState          :: Maybe Text
    , identityPostalCode     :: Maybe Text
    , identityCountry        :: Maybe Text
    , identityCompany        :: Maybe Text
    , identityEmail          :: Maybe Text
    , identityPhone          :: Maybe Text
    , identitySsn            :: Maybe Text
    , identityPassportNumber :: Maybe Text
    , identityUsername       :: Maybe Text
    , identityLicenseNumber  :: Maybe Text
    }
    deriving stock (Show, Eq, Ord)

instance FromJSON Identity where
    parseJSON = withObject "Identity" $ \o -> do
        identityTitle <- o .: "title"
        identityFirstName <- o .: "firstName"
        identityMiddleName <- o .: "middleName"
        identityLastName <- o .: "lastName"
        identityAddress1 <- o .: "address1"
        identityAddress2 <- o .: "address2"
        identityAddress3 <- o .: "address3"
        identityCity <- o .: "city"
        identityState <- o .: "state"
        identityPostalCode <- o .: "postalCode"
        identityCountry <- o .: "country"
        identityCompany <- o .: "company"
        identityEmail <- o .: "email"
        identityPhone <- o .: "phone"
        identitySsn <- o .: "ssn"
        identityUsername <- o .: "username"
        identityPassportNumber <- o .: "passportNumber"
        identityLicenseNumber <- o .: "licenseNumber"
        return Identity{..}

instance ToJSON Identity where
    toJSON Identity{..} =
        object
            [ "title" .= identityTitle
            , "firstName" .= identityFirstName
            , "middleName" .= identityMiddleName
            , "lastName" .= identityLastName
            , "address1" .= identityAddress1
            , "address2" .= identityAddress2
            , "address3" .= identityAddress3
            , "city" .= identityCity
            , "state" .= identityState
            , "postalCode" .= identityPostalCode
            , "country" .= identityCountry
            , "company" .= identityCompany
            , "email" .= identityEmail
            , "phone" .= identityPhone
            , "ssn" .= identitySsn
            , "username" .= identityUsername
            , "passportNumber" .= identityPassportNumber
            , "licenseNumber" .= identityLicenseNumber
            ]

instance FromJSON ItemTemplate where
    parseJSON = withObject "ItemTemplate" $ \o -> do
        itId <- o .: "id"
        itOrgId <- o .: "organizationId"
        itCollectionIds <- o .: "collectionIds"
        itType <- o .: "type"
        itFolderId <- o .: "folderId"
        itName <- o .: "name"
        itNotes <- o .: "notes"
        itFavorite <- o .: "favorite"
        itFields <- o .:? "fields"
        reprompt :: Int <- o .: "reprompt"
        let itRePrompt = case reprompt of
                1 -> True
                _ -> False
        itItem <- case itType of
            1 -> ItemLogin <$> o .: "login"
            2 -> ItemSecureNote <$> o .: "secureNote"
            3 -> ItemCard <$> o .: "card"
            4 -> ItemIdentity <$> o .: "identity"
            _ -> fail "Invalid item type"
        return $ ItemTemplate{..}

instance ToJSON ItemTemplate where
    toJSON ItemTemplate{..} =
        object
            [ "folderId" .= itFolderId
            , "name" .= itName
            , "notes" .= itNotes
            , "favorite" .= itFavorite
            , "id" .= itId
            , "organizationId" .= itOrgId
            , "collectionIds" .= itCollectionIds
            , "type" .= itType
            , "fields" .= itFields
            , "reprompt" .= (if itRePrompt then 1 else 0 :: Int)
            , case itItem of
                ItemLogin l -> "login" .= l
                ItemSecureNote _ -> "secureNote" .= object ["type" .= (0 :: Int)]
                ItemCard c -> "card" .= c
                ItemIdentity i -> "identity" .= i
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
        <$> [ paste' "Name" (if Text.empty == itName then Nothing else Just itName)
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
                        [ ( "URL" <> Text.pack (show i) <> ": " <> unUri url
                          , lift (openInBrowser_ (unUri url))
                          )
                        | (i, url) <- uris
                        ]
                    Nothing -> []
        ItemCard Card{..} ->
            [ paste' "Cardholder Name" cardHolderName
            , paste' "Number" cardNumber
            , paste' "Expiration Month" cardExpMonth
            , paste' "Expiration Year" cardExpYear
            , paste' "Security Code" cardCode
            ]
        ItemIdentity Identity{..} ->
            [ paste' "Title" identityTitle
            , paste' "First Name" identityFirstName
            , paste' "Middle Name" identityMiddleName
            , paste' "Last Name" identityLastName
            , paste' "Address 1" identityAddress1
            , paste' "Address 2" identityAddress2
            , paste' "Address 3" identityAddress3
            , paste' "City" identityCity
            , paste' "State" identityState
            , paste' "Postal Code" identityPostalCode
            , paste' "Country" identityCountry
            , paste' "Company" identityCompany
            , paste' "Email" identityEmail
            , paste' "Phone" identityPhone
            , paste' "SSN" identitySsn
            , paste' "Username" identityUsername
            , paste' "Passport Number" identityPassportNumber
            , paste' "License Number" identityLicenseNumber
            ]
        ItemSecureNote _ -> []

paste' :: Text -> Maybe Text -> (Text, App ())
paste' k v = (k <> ": " <> fromMaybe "None" v, lift (for_ v paste))

data EditMode = Update | Create

editItemI :: EditMode -> ItemTemplate -> Interaction
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
    editItemOptions :: [(Option, App Interaction)]
    editItemOptions =
        ( first Option
            <$> [ editEntry' "Name*" itName [Option itName] (\new -> item{itName = new})
                ,
                    ( "Folder: " <> fromMaybe "None" itFolderId
                    , do
                        folders <- getFolders
                        pure $
                            editI
                                "Folder"
                                (Option . coerce <$> folders)
                                ( edit (\new -> traceShow ("---- NEW: " <> new) (item{itFolderId = if new == "No Folder" then Nothing else Just new}))
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

    miscOptions :: [(Option, App Interaction)]
    miscOptions = case et of
        Create -> [(Option "Save entry", traceShowM (toJSON item) >> callApi ((vaultClient // itemsEp // addItemEp) item) >> pure InteractionEnd)]
        Update ->
            [ (Option "Delete entry", traceShowM itId >> callApi ((vaultClient // itemsEp // deleteItemEp) itId) >> pure InteractionEnd)
            , (Option "Save entry", traceShowM item >> callApi ((vaultClient // itemsEp // editItemEp) itId item) >> pure InteractionEnd)
            ]

    specificEditItemOptions :: [(Option, App Interaction)]
    specificEditItemOptions =
        first Option <$> case itItem of
            (ItemLogin l@Login{..}) ->
                [ editEntry'
                    "Username"
                    (fromMaybe "None" loginUsername)
                    (maybe [] (pure . Option) loginUsername)
                    (\new -> item{itItem = ItemLogin l{loginUsername = Just new}})
                ,
                    ( "Password: " <> maybe "None" (const "********") loginPassword
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
                ,
                    ( "URLs: " <> maybe "None" (const "<Enter to edit>") loginUris
                    , do
                        Menu menu <- asks envMenu
                        url <-
                            lift $
                                menu
                                    (Prompt [ArgPrompt "URL (type to add new)"])
                                    (Option . unUri <$> fromMaybe [] loginUris)
                        case url of
                            Left newUrl ->
                                pure
                                    ( editItemI
                                        et
                                        (item{itItem = ItemLogin l{loginUris = (Uri newUrl Nothing :) <$> loginUris}})
                                    )
                            Right (Option oldUrl) -> do
                                let urlDeleted = filter ((/= oldUrl) . unUri) <$> loginUris
                                let urlActions = [(Option oldUrl, pure (editItemI et item)), (Option "Delete", pure (editItemI et (item{itItem = ItemLogin l{loginUris = urlDeleted}})))]

                                url' <- lift $ menu (Prompt [ArgPrompt "URL"]) (fst <$> urlActions)
                                case url' of
                                    Left changedUrl ->
                                        pure
                                            ( editItemI
                                                et
                                                (item{itItem = ItemLogin l{loginUris = (Uri changedUrl Nothing :) <$> urlDeleted}})
                                            )
                                    Right (Option opt) -> case lookup (Option opt) urlActions of
                                        Just action -> action
                                        Nothing     -> pure (editItemI et item)
                    )
                ]
            ItemCard c@Card{..} ->
                [ editEntry ItemCard "Cardholder Name" cardHolderName (\new -> c{cardHolderName = new})
                , editEntry ItemCard "Number" cardNumber (\new -> c{cardNumber = new})
                , editEntry ItemCard "Security Code" cardCode (\new -> c{cardCode = new})
                ]
            ItemIdentity i@Identity{..} ->
                [ editEntry ItemIdentity "Title" identityTitle (\new -> i{identityTitle = new})
                , editEntry ItemIdentity "First Name" identityFirstName (\new -> i{identityFirstName = new})
                , editEntry ItemIdentity "Middle Name" identityMiddleName (\new -> i{identityMiddleName = new})
                , editEntry ItemIdentity "Last Name" identityLastName (\new -> i{identityLastName = new})
                , editEntry ItemIdentity "Address 1" identityAddress1 (\new -> i{identityAddress1 = new})
                , editEntry ItemIdentity "Address 2" identityAddress2 (\new -> i{identityAddress2 = new})
                , editEntry ItemIdentity "Address 3" identityAddress3 (\new -> i{identityAddress3 = new})
                , editEntry ItemIdentity "City" identityCity (\new -> i{identityCity = new})
                , editEntry ItemIdentity "State" identityState (\new -> i{identityState = new})
                , editEntry ItemIdentity "Postal Code" identityPostalCode (\new -> i{identityPostalCode = new})
                , editEntry ItemIdentity "Country" identityCountry (\new -> i{identityCountry = new})
                , editEntry ItemIdentity "Company" identityCompany (\new -> i{identityCompany = new})
                , editEntry ItemIdentity "Email" identityEmail (\new -> i{identityEmail = new})
                , editEntry ItemIdentity "Phone" identityPhone (\new -> i{identityPhone = new})
                , editEntry ItemIdentity "SSN" identitySsn (\new -> i{identitySsn = new})
                , editEntry ItemIdentity "Passport Number" identityPassportNumber (\new -> i{identityPassportNumber = new})
                , editEntry ItemIdentity "Username" identityUsername (\new -> i{identityUsername = new})
                , editEntry ItemIdentity "License Number" identityLicenseNumber (\new -> i{identityLicenseNumber = new})
                ]
            ItemSecureNote _ -> []

    editEntry' :: (Applicative f) => Text -> Text -> [Option] -> (Text -> ItemTemplate) -> (Text, f Interaction)
    editEntry' k current vs withItem = (k <> ": " <> current, pure $ editI k vs (edit withItem))

    edit :: (Text -> ItemTemplate) -> Either Text Option -> App Interaction
    edit withNew e = pure $ case e of
        Right (Option o) -> editItemI et (withNew o)
        Left new         -> editItemI et (withNew new)

    editEntry :: (Applicative f) => (t -> Item) -> Text -> Maybe Text -> (Maybe Text -> t) -> (Text, f Interaction)
    editEntry it k v withItem =
        editEntry'
            k
            (fromMaybe "None" v)
            (maybe [] (pure . Option) v)
            (\new -> item{itItem = it (withItem (Just new))})

editI :: (Coercible a [Option]) => Text -> a -> (Either Text Option -> App Interaction) -> Interaction
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
                Nothing -> announce "No note"
            ItemCard (Card{..}) ->
                lift
                    ( openInEditor_
                        ( Text.unlines
                            ( catMaybes
                                [ cardHolderName
                                , cardNumber
                                , cardExpMonth
                                , cardExpYear
                                , cardCode
                                ]
                            )
                        )
                    )
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
                    , pure $
                        editItemI
                            Create
                            ( emptyItem
                                ( ItemLogin
                                    ( Login
                                        { loginUris = Nothing
                                        , loginUsername = Nothing
                                        , loginPassword = Nothing
                                        , loginTotp = Nothing
                                        }
                                    )
                                )
                            )
                    )
                ,
                    ( "Identity"
                    , pure $
                        editItemI
                            Create
                            ( emptyItem
                                ( ItemIdentity
                                    ( Identity
                                        -- :)
                                        Nothing
                                        Nothing
                                        Nothing
                                        Nothing
                                        Nothing
                                        Nothing
                                        Nothing
                                        Nothing
                                        Nothing
                                        Nothing
                                        Nothing
                                        Nothing
                                        Nothing
                                        Nothing
                                        Nothing
                                        Nothing
                                        Nothing
                                        Nothing
                                    )
                                )
                            )
                    )
                , ("Secure Note", pure $ editItemI Create (emptyItem (ItemSecureNote SecureNote)))
                , ("Card", pure $ editItemI Create (emptyItem (ItemCard (Card Nothing Nothing Nothing Nothing Nothing))))
                ]

    emptyItem :: Item -> ItemTemplate
    emptyItem item =
        ItemTemplate
            { itId = ""
            , itOrgId = Nothing
            , itCollectionIds = []
            , itFolderId = Nothing
            , itType = itemToType item
            , itName = ""
            , itFields = Nothing
            , itRePrompt = False
            , itNotes = Nothing
            , itFavorite = False
            , itItem = item
            }

announceI :: Text -> Interaction
announceI a =
    InteractionQuestion $
        Question
            (Prompt [ArgPrompt a])
            []
            (const $ pure InteractionEnd)

itemToType :: Item -> Int
itemToType item = case item of
    ItemLogin _      -> 1
    ItemSecureNote _ -> 2
    ItemCard _       -> 3
    ItemIdentity _   -> 4

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
openInBrowser_ = run_ "firefox" . pure

openInEditor :: Text -> Sh Text
openInEditor prevNotes = do
    editor <- get_env "EDITOR"
    case editor of
        Nothing -> errorExit "EDITOR not set"
        Just _e -> withTmpDir $ \fp -> do
            writefile (fp <.> "hwarden") prevNotes
            run_ "st" ["nano", toTextIgnore (fp <.> "hwarden")] -- TODO: fix options for nvim
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
                    let gen spec = Just . coerce <$> callApi ((vaultClient // miscEp // generatePasswordEp) (Just len') False False False spec)
                    case s of
                        "Yes" -> gen True
                        "No"  -> gen False
                        _     -> pure Nothing
        _ -> undefined

-- doesn't show password in logs
-- signifcantly faster, uses bw serve
-- case insensitivty
-- Force user to choose name first for any item (required)
