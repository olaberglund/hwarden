{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DerivingStrategies   #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}

module Api where

import           Control.Monad  ((>=>))
import           Data.Aeson     (FromJSON (parseJSON), ToJSON (toJSON), decode,
                                 object, withObject, withText, (.:), (.=))
import           Data.Bifunctor (Bifunctor (first))
import           Data.Coerce    (coerce)
import           Data.Maybe     (fromMaybe)
import           Data.Text      (Text)
import qualified Data.Text      as Text
import           Data.Time      (UTCTime)
import           Debug.Trace
import           GHC.Generics   (Generic)
import           Item           (Item (..), ItemTemplate)
import           Prelude        hiding (log)
import           Servant        (Capture, Delete, Get, JSON, NamedRoutes, Post,
                                 PostNoContent, Proxy (Proxy), Put, QueryFlag,
                                 QueryParam, ReqBody, (:-), (:>))
import           Servant.Client (AsClientT, ClientEnv, ClientError (..),
                                 ClientM, Response, client, responseBody,
                                 runClientM)

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
            . traceShowId

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

data VaultFoldersApi as = VaultFoldersApi
    { getFoldersEp :: as :- "list" :> "object" :> "folders" :> Get '[JSON] (VaultResponse FoldersData)
    , addFolderEp :: as :- "object" :> "folder" :> ReqBody '[JSON] FolderName :> Post '[JSON] (VaultResponse Folder)
    , editFolderEp :: as :- "object" :> "folder" :> Capture "id" Text :> ReqBody '[JSON] FolderName :> Put '[JSON] (VaultResponse Folder)
    , deleteFolderEp :: as :- "object" :> "folder" :> Capture "id" Text :> Delete '[JSON] EmptyVaultResponse
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

newtype GenerateData = GenerateData
    { unGenerateData :: Text
    }
    deriving stock (Show, Eq)

instance FromJSON GenerateData where
    parseJSON = withObject "GenerateData" $
        \o -> GenerateData <$> o .: "data"

callApiIO :: ClientM a -> ClientEnv -> IO (Either Text a)
callApiIO action env = first handleClientError <$> runClientM action env

newtype FoldersData = FoldersData
    { unFoldersData :: [Folder]
    }
    deriving stock (Show, Eq)

instance FromJSON FoldersData where
    parseJSON = withObject "FoldersData" $ \o -> FoldersData <$> o .: "data"

data Folder = Folder
    { folderId   :: Maybe Text -- The 'No Folder' has no id
    , folderName :: Text
    }
    deriving (Show, Eq)

instance FromJSON Folder where
    parseJSON = withObject "Folder" $ \o -> do
        folderId <- o .: "id"
        folderName <- o .: "name"
        return Folder{..}

instance ToJSON Folder where
    toJSON Folder{..} = object ["name" .= folderName, "id" .= folderId]

newtype FolderName = FolderName
    { unFolderName :: Text
    }
    deriving (Show, Eq)

instance ToJSON FolderName where
    toJSON (FolderName name) = object ["name" .= name]

itemToType :: Item -> Int
itemToType item = case item of
    ItemLogin _      -> 1
    ItemSecureNote _ -> 2
    ItemCard _       -> 3
    ItemIdentity _   -> 4
