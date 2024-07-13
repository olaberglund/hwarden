module Main where

import           Api
import           Control.Monad              (when)
import           Control.Monad.Trans.Reader (mapReaderT, runReaderT)
import           Env
import           Interaction
import           Menu
import           Network.HTTP.Client        (defaultManagerSettings, newManager)
import           Prelude                    hiding (log)
import           Servant.Client             (mkClientEnv, parseBaseUrl)
import           Shelly                     (shelly, silently, verbosely)

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

-- doesn't show password in logs
-- signifcantly faster, uses bw serve
-- case insensitivty
-- Force user to choose name first for any item (required)
