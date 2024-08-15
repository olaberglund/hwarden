{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Interaction (
    dashboardI,
    getItems,
    getStatus,
    unlockI,
    interactionLoop,
    unlock,
    announce,
) where

import           Api
import           Control.Monad              (replicateM_, void)
import           Control.Monad.Trans.Class  (lift)
import           Control.Monad.Trans.Reader (ReaderT, asks, mapReaderT)
import           Data.Bifunctor             (Bifunctor (first), bimap)
import           Data.Coerce                (Coercible, coerce)
import           Data.Foldable              (for_)
import           Data.Functor               ((<&>))
import           Data.List                  (find)
import           Data.Maybe                 (catMaybes, fromMaybe, isJust)
import           Data.Text                  (Text)
import qualified Data.Text                  as Text
import           Env
import           Item
import           Menu
import           Prelude                    hiding (log)
import           Servant.Client             (ClientM, (//))
import           Shelly                     (Sh, errorExit, exit, fromText,
                                             get_env, liftIO, readfile, run_,
                                             toTextIgnore, withTmpDir,
                                             writefile, (<.>))
import           Text.Read                  (readMaybe)

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

unlockI :: Interaction
unlockI =
    InteractionQuestion $
        Question
            (Prompt [ArgPrompt "Enter Password", ArgObscured])
            []
            ( \case
                Right _ -> pure InteractionEnd
                Left pw -> unlock (Password pw) >> pure InteractionEnd
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
                    case cachedItem of
                        Just item -> pure (typeItemI item)
                        Nothing -> announce "No previous entry" >> pure (dashboardI items)
                )
            , ("Edit entries", allItemsI (editItemI Update) <$> getItems)
            , ("Add entry", pure addNewItemI)
            , ("Manage folders", pure manageFoldersI)
            , ("Manage collections", pure manageCollectionsI)
            , ("Sync vault", sync >> announce "Vault synced" >> pure (dashboardI items))
            , ("Lock vault", lock >> announce "Vault locked" >> pure InteractionEnd)
            , ("Log out", lift (run_ "bw" ["logout"]) >> announce "Logged out" >> pure InteractionEnd)
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

callApi :: ClientM a -> ReaderT Env Sh a
callApi action = do
    env <- asks envClient
    liftIO (callApiIO action env) >>= \case
        Left e -> announce e >> lift (exit 1)
        Right r -> pure r

announce :: Text -> App ()
announce = interactionLoop . announceI

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
                                (Option . folderName <$> folders)
                                ( edit (\new -> item{itFolderId = if new == "No Folder" then Nothing else Just new})
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
        Create ->
            [
                ( Option "Save entry"
                , do
                    void $ callApi ((vaultClient // itemsEp // addItemEp) item)
                    announce "Entry created"
                    getItems <&> dashboardI
                )
            ]
        Update ->
            [
                ( Option "Delete entry"
                , do
                    void $ callApi ((vaultClient // itemsEp // deleteItemEp) itId)
                    announce "Entry Deleted"
                    getItems <&> dashboardI
                )
            ,
                ( Option "Save entry"
                , do
                    void $ callApi ((vaultClient // itemsEp // editItemEp) itId item)
                    announce "Changes saved"
                    pure (editItemI et item)
                )
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

manageCollectionsI :: Interaction
manageCollectionsI =
    InteractionQuestion $
        Question
            (Prompt [ArgPrompt "Manage Collections"])
            (fst <$> options)
            ( \case
                Left _ -> pure InteractionEnd
                Right i -> case i `lookup` options of
                    Nothing -> pure InteractionEnd
                    Just i' -> i'
            )
  where
    options :: [(Option, App Interaction)]
    options =
        [
            ( Option "Create"
            , do
                Menu menu <- asks envMenu
                status <- getStatus
                organizations <- coerce <$> callApi (vaultClient // collectionsEp // getOrganizationsEp)
                let orgs = (,) <$> organizationName <*> organizationId <$> organizations
                Right (Option chosenOrg) <- lift $ menu (Prompt [ArgPrompt "Select Organization"]) (Option . organizationName <$> organizations)
                Left name <- lift $ menu (Prompt [ArgPrompt "Collection Name"]) []
                case chosenOrg `lookup` orgs of
                    Just orgId -> do
                        void $
                            callApi
                                ( (vaultClient // collectionsEp // addCollectionEp)
                                    (Just orgId)
                                    (CollectionBody orgId name [] [CollectionMember (statusUserId status) False False True])
                                )
                        announce $ "Collection created: " <> name
                        pure manageFoldersI
                    Nothing -> pure $ announceI "Invalid organization"
            )
        ]

manageFoldersI :: Interaction
manageFoldersI =
    InteractionQuestion $
        Question
            (Prompt [ArgPrompt "Manage Folders"])
            (fst <$> options)
            ( \case
                Left _ -> pure InteractionEnd
                Right i -> case i `lookup` options of
                    Nothing -> pure InteractionEnd
                    Just i' -> i'
            )
  where
    options :: [(Option, App Interaction)]
    options =
        [
            ( Option "Create"
            , do
                Menu menu <- asks envMenu
                Left name <- lift $ menu (Prompt [ArgPrompt "Folder name"]) []
                void $ callApi ((vaultClient // foldersEp // addFolderEp) (FolderName name))
                pure manageFoldersI
            )
        ,
            ( Option "Rename"
            , do
                Menu menu <- asks envMenu
                folder <- pickFolder "Rename folder"
                case folderId folder of
                    Just fid -> do
                        opt <- lift $ menu (Prompt [ArgPrompt "Rename folder"]) [Option (folderName folder)]
                        case opt of
                            Left name -> case name of
                                "" -> pure $ announceI "Name can not be empty"
                                name' -> do
                                    void $ callApi ((vaultClient // foldersEp // editFolderEp) fid (FolderName name'))
                                    pure manageFoldersI
                            _ -> pure manageFoldersI
                    Nothing -> pure manageFoldersI
            )
        ,
            ( Option "Delete"
            , do
                Menu menu <- asks envMenu
                folders <- getFolders
                let folderOptions = map ((,) <$> folderName <*> folderId) folders
                Right (Option name) <- lift $ menu (Prompt [ArgPrompt "Folder name"]) (Option . fst <$> folderOptions)
                case name `lookup` folderOptions of
                    Just fid -> do
                        void $ callApi ((vaultClient // foldersEp // deleteFolderEp) (fromMaybe "" fid))
                        pure manageFoldersI
                    Nothing -> pure manageFoldersI
            )
        ]

unlock :: Password -> App UnlockData
unlock = mapReaderT (fmap coerce) . callApi . (vaultClient // lockingEp // unlockEp)

lock :: App TitledData
lock = mapReaderT (fmap coerce) $ callApi (vaultClient // lockingEp // lockEp)

sync :: App TitledData
sync = mapReaderT (fmap coerce) $ callApi (vaultClient // miscEp // syncEp)

getItems :: App [ItemTemplate]
getItems = mapReaderT (fmap coerce) $ callApi (vaultClient // itemsEp // getItemsEp)

getStatus :: App Status
getStatus = mapReaderT (fmap coerce) $ callApi (vaultClient // miscEp // statusEp)

getFolders :: App [Folder]
getFolders = mapReaderT (fmap coerce) $ callApi (vaultClient // foldersEp // getFoldersEp)

pickFolder :: Text -> App Folder
pickFolder prompt = do
    Menu menu <- asks envMenu
    folders <- getFolders
    Right (Option f) <- lift $ menu (Prompt [ArgPrompt prompt]) (Option . folderName <$> filter (isJust . folderId) folders)
    case find ((== f) . folderName) folders of
        Just folder -> pure folder
        Nothing     -> pickFolder prompt
