{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module FreeInteraction (app', interpret) where

import           Api
import           Control.Arrow              (first)
import           Control.Concurrent         (threadDelay)
import           Control.Concurrent.Async   (async, wait, withAsync)
import           Control.Monad              (void)
import           Control.Monad.Free
import           Control.Monad.Trans.Class  (lift)
import           Control.Monad.Trans.Reader
import           Data.Aeson                 (decode)
import qualified Data.ByteString.Lazy       as BL
import           Data.Coerce                (coerce)
import           Data.Foldable              (for_)
import           Data.Functor               (($>), (<&>))
import           Data.Maybe                 (fromMaybe)
import           Data.Text                  (Text)
import qualified Data.Text                  as Text
import qualified Data.Text.Encoding         as T
import           Env                        (App, Env (..))
import           Interaction                (announce, getItems, unlock,
                                             withCacheFile)
import           Item
import           Menu
import           Servant.Client             (ClientEnv, runClientM, (//))
import           Server
import           Shelly
import           System.Timeout             (timeout)

data Question = Question
    { questionPrompt  :: Prompt
    , questionOptions :: [Option]
    }

data InteractionF next
    = Ask Question (Either Text Option -> next)
    | LogIn Text Password next
    | GetServerStatus (ServerStatus -> next)
    | TryServe (Maybe ServerStatus -> next)
    | GetItems ([ItemTemplate] -> next)
    | Announce Text next
    | Unlock Password next
    | WriteCache Text next
    | Paste Text next
    | OpenInEditor Text (Text -> next)
    | OpenInBrowser Uri next
    deriving (Functor)

type Interaction' = Free InteractionF

askQuestionM :: Question -> App (Either Text Option)
askQuestionM q = do
    Menu menu <- asks envMenu
    lift (menu (questionPrompt q) (questionOptions q))

askQuestion' :: Question -> Interaction' (Either Text Option)
askQuestion' = liftF . flip Ask id

askQuestionR :: Text -> [(Option, Interaction' b)] -> Interaction' (Maybe b)
askQuestionR msg conts = do
    let options = fst <$> conts
    ans <- askQuestion' (Question (Prompt [ArgPrompt msg]) options)
    case ans of
        Left _ -> pure Nothing
        Right opt -> case opt `lookup` conts of
            Just cont -> pure <$> cont
            Nothing   -> pure Nothing

logIn :: Text -> Password -> Interaction' ()
logIn email pw = liftF $ LogIn email pw ()

askPassword :: Interaction' (Maybe Password)
askPassword = do
    askQuestion' (Question (Prompt [ArgPrompt "Enter Password", ArgObscured]) []) >>= \case
        Left pw | Text.empty /= pw -> pure $ Just (Password pw)
        _ -> pure Nothing

askEmail :: Interaction' (Maybe Text)
askEmail =
    askQuestion' (Question (Prompt [ArgPrompt "Enter Email"]) []) >>= \case
        Left email | Text.empty /= email -> pure (Just email)
        _ -> pure Nothing

announce' :: Text -> Interaction' ()
announce' = liftF . flip Announce ()

tryServe :: Interaction' (Maybe ServerStatus)
tryServe = liftF $ TryServe id

getServerStatus :: Interaction' ServerStatus
getServerStatus = liftF $ GetServerStatus id

getItems' :: Interaction' [ItemTemplate]
getItems' = liftF $ GetItems id

unlock' :: Password -> Interaction' ()
unlock' = liftF . flip Unlock ()

app' :: Interaction' ()
app' = getServerStatus >>= actOnStatus'

writeCache' :: Text -> Interaction' ()
writeCache' = liftF . flip WriteCache ()

actOnStatus' :: ServerStatus -> Interaction' ()
actOnStatus' = \case
    ServerOffline Undecided -> announce' "Unexpected status response from bitwarden"
    ServerOffline Unauthenticated ->
        askEmail >>= \case
            Nothing -> announce' "Empty Email"
            Just email ->
                askPassword >>= \case
                    Nothing -> announce' "Empty Password"
                    Just pw -> do
                        logIn email pw
                        actOnStatus' =<< getServerStatus
    ServerOffline Authenticated -> do
        mstatus <- tryServe
        case mstatus of
            Just s  -> actOnStatus' s
            Nothing -> announce' "Failed to start"
    ServerOnline Unlocked -> do
        items <- getItems'
        dashboard' items
    ServerOnline Locked -> do
        mpw <- askPassword
        case mpw of
            Nothing -> announce' "Empty Password"
            Just pw -> do
                unlock' pw
                actOnStatus' =<< getServerStatus

paste :: Text -> Interaction' ()
paste = liftF . flip Paste ()

paste' :: Text -> Maybe Text -> (Text, Interaction' ())
paste' k v = (k <> ": " <> fromMaybe "None" v, for_ v paste)

openInEditor :: Text -> Interaction' Text
openInEditor = liftF . flip OpenInEditor id

openInBrowser :: Uri -> Interaction' ()
openInBrowser = liftF . flip OpenInBrowser ()

typeItem' :: ItemTemplate -> Interaction' ()
typeItem' item = void $ askQuestionR "Entries" (typeItemOptions' item)

allItems' :: (ItemTemplate -> Interaction' ()) -> [ItemTemplate] -> Interaction' ()
allItems' interaction items = void (askQuestionR "Entries" continuations)
  where
    continuations :: [(Option, Interaction' ())]
    continuations = (\t -> (Option (toEntry t), writeCache' (itId t) >> interaction t)) <$> items

typeItemOptions' :: ItemTemplate -> [(Option, Interaction' ())]
typeItemOptions' (ItemTemplate{..}) =
    map (first Option) $
        [ paste' "Name" (if Text.empty == itName then Nothing else Just itName)
        , ("Folder: " <> fromMaybe "None" itFolderId, mapM_ paste itFolderId)
        ,
            ( "Notes: " <> maybe "None" (const "<Enter to view>") itNotes
            , mapM_ openInEditor itNotes
            )
        ]
            <> specificTypeItemOptions itItem
  where
    specificTypeItemOptions :: Item -> [(Text, Interaction' ())]
    specificTypeItemOptions item =
        case item of
            ItemLogin Login{..} ->
                [ ("Username: " <> fromMaybe "None" loginUsername, mapM_ paste loginUsername)
                , ("Password: ********", mapM_ paste loginPassword)
                , ("TOTP: " <> fromMaybe "None" loginTotp, pure ())
                ]
                    <> case fmap (zip [1 :: Int ..]) loginUris of
                        Just uris ->
                            [ ("URL" <> Text.pack (show i) <> ": " <> unUri url, openInBrowser url)
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

dashboard' :: [ItemTemplate] -> Interaction' ()
dashboard' items = void $ askQuestionR "Entries" continuations
  where
    continuations :: [(Option, Interaction' ())]
    continuations =
        map
            (first Option)
            [ ("View/type individual items", allItems' typeItem' items)
            ,
                ( "View previous entry"
                , Pure ()
                )
            , ("Edit entries", Pure ())
            , ("Add entry", Pure ())
            , ("Manage folders", Pure ())
            , ("Manage collections", Pure ())
            , ("Sync vault", Pure ())
            , ("Lock vault", Pure ())
            , ("Log out", Pure ())
            , ("-------- Quick actions --------", Pure ())
            ]
            <> map (\item -> (Option (toEntry item), Pure ())) items

interpret :: Interaction' a -> App a
interpret = foldFree morph
  where
    morph :: InteractionF a -> App a
    morph = \case
        Ask q next -> next <$> askQuestionM q
        GetServerStatus next -> getServerStatusApp <&> next
        LogIn email pw next -> do
            lift (tracing False $ run_ "bw" ["login", "--nointeraction", email, coerce pw])
            pure next
        TryServe next -> do
            client <- asks envClient
            status <- liftIO $
                timeout 10_000_000 $
                    withAsync (pollServer client) $ \poll -> do
                        void $ async (shelly $ run_ "bw" ["serve"])
                        wait poll
            pure (next status)
        Announce msg next -> announce msg >> pure next
        Unlock pw next -> unlock pw >> pure next
        GetItems next -> next <$> getItems
        WriteCache txt next -> lift (withCacheFile (`writefile` txt)) $> next
        Paste text next -> lift (run_ "xdotool" ["type", "--delay", "0", text]) $> next
        OpenInEditor text next -> lift $ do
            editor <- get_env "EDITOR"
            content <- case editor of
                Nothing -> errorExit "EDITOR not set"
                Just _e -> withTmpDir $ \fp -> do
                    writefile (fp <.> "hwarden") text
                    run_ "st" ["nano", toTextIgnore (fp <.> "hwarden")]
                    readfile (fp <.> "hwarden")
            pure (next content)
        OpenInBrowser uri next -> lift (run_ "firefox" [unUri uri]) $> next

pollServer :: ClientEnv -> IO ServerStatus
pollServer client = do
    res <- fmap unVaultResponse <$> runClientM (vaultClient // miscEp // statusEp) client
    case res of
        Left _  -> threadDelay 50_000 >> pollServer client
        Right s -> pure (ServerOnline (statusLockStatus s))

getServerStatusApp :: App ServerStatus
getServerStatusApp = do
    client <- asks envClient
    status <- liftIO (fmap coerce <$> runClientM (vaultClient // miscEp // statusEp) client)
    case status of
        Right s -> pure (ServerOnline (statusLockStatus s))
        Left _ -> do
            statusJson <- lift (print_stdout False $ run "bw" ["status"])
            let mstatus = decode . BL.fromStrict . T.encodeUtf8 $ statusJson
            pure . ServerOffline $ case mstatus of
                Just (BwStatus bs) -> bs
                Nothing            -> Undecided
