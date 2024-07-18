module Env (Env (..), App) where

import           Control.Monad.Trans.Reader (ReaderT)
import           Data.Text                  (Text)
import           Menu                       (Menu)
import           Prelude                    hiding (log)
import           Servant.Client             (ClientEnv)
import           Shelly                     (Sh)

data Env = Env
    { envClient :: ClientEnv
    , envMenu   :: Menu
    , envLog    :: Text -> App ()
    }

type App = ReaderT Env Sh
