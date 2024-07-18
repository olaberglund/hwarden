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

type Todo = PostNoContent

data VaultApi as = VaultApi
    { lockingEp     :: as :- NamedRoutes VaultLockApi
    , itemsEp       :: as :- NamedRoutes VaultItemsApi
    , foldersEp     :: as :- NamedRoutes VaultFoldersApi
    , miscEp        :: as :- NamedRoutes VaultMiscellaneousApi
    , collectionsEp :: as :- NamedRoutes VaultCollectionsApi
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
    { getFoldersEp :: as :- "list" :> "object" :> "folders" :> Get '[JSON] (VaultResponse (DataObject [Folder]))
    , addFolderEp :: as :- "object" :> "folder" :> ReqBody '[JSON] FolderName :> Post '[JSON] (VaultResponse Folder)
    , editFolderEp :: as :- "object" :> "folder" :> Capture "id" Text :> ReqBody '[JSON] FolderName :> Put '[JSON] (VaultResponse Folder)
    , deleteFolderEp :: as :- "object" :> "folder" :> Capture "id" Text :> Delete '[JSON] EmptyVaultResponse
    }
    deriving stock (Generic)

data VaultCollectionsApi as = VaultCollectionsApi
    { addCollectionEp :: as :- "object" :> "org-collection" :> QueryParam "organizationId" Text :> ReqBody '[JSON] CollectionBody :> Post '[JSON] (VaultResponse Collection)
    , getOrganizationsEp :: as :- "list" :> "object" :> "organizations" :> Get '[JSON] (VaultResponse (DataObject [Organization]))
    }
    deriving stock (Generic)

data VaultMiscellaneousApi as = VaultMiscellaneousApi
    { statusEp :: as :- "status" :> Get '[JSON] (VaultResponse Status)
    , syncEp :: as :- "sync" :> Post '[JSON] (VaultResponse TitledData)
    , generatePasswordEp ::
        as
            :- "generate"
                :> QueryParam "length" Int
                :> QueryFlag "uppercase"
                :> QueryFlag "lowercase"
                :> QueryFlag "number"
                :> QueryFlag "special"
                :> Get '[JSON] (VaultResponse (DataObject Text))
    }
    deriving stock (Generic)

data Status = Status
    { statusServerUrl  :: Maybe Text
    , statusLastSync   :: UTCTime
    , statusUserEmail  :: Text
    , statusUserId     :: Text
    , statusLockStatus :: LockStatus
    }
    deriving stock (Show, Eq)

data LockStatus = Locked | Unlocked
    deriving stock (Show, Eq)

instance FromJSON LockStatus where
    parseJSON = withText "LockStatus" $ \case
        "locked" -> pure Locked
        "unlocked" -> pure Unlocked
        s -> fail ("Invalid lock status: " <> Text.unpack s)

instance FromJSON Status where
    parseJSON =
        withObject "Status" $
            (.: "template") >=> \t -> do
                statusServerUrl <- t .: "serverUrl"
                statusLastSync <- t .: "lastSync"
                statusUserEmail <- t .: "userEmail"
                statusUserId <- t .: "userId"
                statusLockStatus <- t .: "status"
                pure Status{..}

newtype ItemsData = ItemsData
    { unItemsData :: [ItemTemplate]
    }
    deriving stock (Show, Eq)

instance FromJSON ItemsData where
    parseJSON = withObject "ItemsData" $
        \o -> ItemsData <$> o .: "data"

callApiIO :: ClientM a -> ClientEnv -> IO (Either Text a)
callApiIO action env = first handleClientError <$> runClientM action env

data Organization = Organization
    { organizationId   :: Text
    , organizationName :: Text
    }
    deriving stock (Show, Eq)

instance FromJSON Organization where
    parseJSON = withObject "Organization" $ \o -> do
        organizationId <- o .: "id"
        organizationName <- o .: "name"
        return Organization{..}

data Collection = Collection
    { collectionOrgId :: Text
    , collectionName  :: Text
    , groups          :: [CollectionGroup]
    }
    deriving stock (Show, Eq)

data CollectionGroup = CollectionGroup
    { collectionGroupId            :: Text
    , collectionGroupReadOnly      :: Bool
    , collectionGroupHidePasswords :: Bool
    }
    deriving stock (Show, Eq)

data CollectionMember = CollectionMember
    { collectionMemberId            :: Text
    , collectionMemberReadOnly      :: Bool
    , collectionMemberHidePasswords :: Bool
    , collectionManage              :: Bool
    }
    deriving stock (Show, Eq)

instance ToJSON CollectionMember where
    toJSON CollectionMember{..} =
        object
            [ "id" .= collectionMemberId
            , "readOnly" .= collectionMemberReadOnly
            , "hidePasswords" .= collectionMemberHidePasswords
            , "manage" .= collectionManage
            ]

data CollectionBody = CollectionBody
    { collectionBodyOrgId            :: Text
    , collectionBodyName             :: Text
    , collectionBodyCollectionGroups :: [CollectionGroup]
    , collectionBodyUsers            :: [CollectionMember]
    }
    deriving stock (Show, Eq)

instance ToJSON CollectionBody where
    toJSON CollectionBody{..} =
        object
            [ "name" .= collectionBodyName
            , "organizationId" .= collectionBodyOrgId
            , "groups" .= collectionBodyCollectionGroups
            , "externalId" .= (Nothing :: Maybe Text)
            , "users" .= collectionBodyUsers
            ]

instance FromJSON Collection where
    parseJSON = withObject "Collection" $ \o -> do
        collectionOrgId <- o .: "orgId"
        collectionName <- o .: "name"
        groups <- o .: "groups"
        return Collection{..}

instance ToJSON CollectionGroup where
    toJSON CollectionGroup{..} =
        object
            [ "id" .= collectionGroupId
            , "readOnly" .= collectionGroupReadOnly
            , "hidePasswords" .= collectionGroupHidePasswords
            ]

instance FromJSON CollectionGroup where
    parseJSON = withObject "CollectionGroup" $ \o -> do
        collectionGroupId <- o .: "id"
        collectionGroupReadOnly <- o .: "readOnly"
        collectionGroupHidePasswords <- o .: "hidePasswords"
        return CollectionGroup{..}

newtype DataObject a = DataObject
    { unDataObject :: a
    }
    deriving stock (Show, Eq)

instance (FromJSON a) => FromJSON (DataObject a) where
    parseJSON = withObject "DataObject" $ \o -> DataObject <$> o .: "data"

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
