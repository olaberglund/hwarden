{-# LANGUAGE DerivingStrategies #-}

module Main where

import           GHC.Generics   (Generic)
import           Servant        (NamedRoutes, PostNoContent, Proxy (Proxy),
                                 (:-))
import           Servant.Client (AsClientT, ClientM, client)

main :: IO ()
main = putStrLn "Hello, Haskell!"

type Todo = PostNoContent

data VaultApi as = VaultApi
    { lockApi       :: as :- NamedRoutes LockApi
    , vaultItemsApi :: as :- NamedRoutes VaultItemsApi
    }
    deriving stock (Generic)

data LockApi as = LockApi
    { lockEp   :: as :- Todo
    , unlockEp :: as :- Todo
    }
    deriving stock (Generic)

data VaultItemsApi as = VaultItemsApi
    { addItemEp     :: as :- Todo
    , editItemEp    :: as :- Todo
    , getItemEp     :: as :- Todo
    , getItemsEp    :: as :- Todo
    , deleteItemEp  :: as :- Todo
    , restoreItemEp :: as :- Todo
    }
    deriving stock (Generic)

data MiscellaneousApi as = MiscellaneousApi
    { syncEp           :: as :- Todo
    , statusEp         :: as :- Todo
    , generatePassword :: as :- Todo
    }
    deriving stock (Generic)

vaultClient :: VaultApi (AsClientT ClientM)
vaultClient = client (Proxy @(NamedRoutes VaultApi))
