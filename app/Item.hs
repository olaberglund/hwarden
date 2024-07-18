{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

module Item (
    ItemTemplate (..),
    Item (..),
    Card (..),
    Login (..),
    Identity (..),
    Uri (..),
    SecureNote (..),
) where

import           Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON), object,
                             withObject, (.:), (.:?), (.=))
import           Data.List  (intersperse)
import           Data.Maybe (catMaybes)
import           Data.Ord   (comparing)
import           Data.Text  (Text)
import qualified Data.Text  as Text
import           Menu
import           Prelude    hiding (log)

data Item
    = ItemLogin Login
    | ItemCard Card
    | ItemSecureNote SecureNote
    | ItemIdentity Identity
    deriving stock (Show, Eq, Ord)

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
    , fieldValue :: Maybe Text
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

toSymbol :: Item -> Text
toSymbol = \case
    ItemLogin _ -> "(l)"
    ItemCard _ -> "(c)"
    ItemIdentity _ -> "(i)"
    ItemSecureNote _ -> "(n)"

merge :: [Text] -> Text
merge = mconcat . intersperse " - "
