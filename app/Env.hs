module Env (Env (..), App) where

import           Control.Monad.Trans.Reader (ReaderT)
import           Menu                       (Menu)
import           Prelude                    hiding (log)
import           Servant.Client             (ClientEnv)
import           Shelly                     (Sh)

data Env = Env
    { envClient :: ClientEnv
    , envMenu   :: Menu
    }

type App = ReaderT Env Sh
