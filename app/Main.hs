module Main where

import           Control.Monad.Trans.Class  (MonadTrans (lift))
import           Control.Monad.Trans.Reader (ReaderT (..))
import           Env
import           FreeInteraction            (app', interpret)
import           GHC.MVar                   (newEmptyMVar)
import           Menu
import           Network.HTTP.Client        (defaultManagerSettings, newManager)
import           Prelude                    hiding (log)
import           Servant.Client             (mkClientEnv, parseBaseUrl)
import           Server
import           Shelly

main :: IO ()
main = do
    manager <- newManager defaultManagerSettings
    baseUrl <- parseBaseUrl "http://localhost:8087"
    let env = mkClientEnv manager baseUrl

    -- shelly $ runReaderT app (Env env (Menu dmenu) (lift . echo))
    shelly $ runReaderT (interpret app') (Env env (Menu dmenu) (lift . echo))

app :: App ()
app = do
    status <- serverStatus
    mpw <- liftIO newEmptyMVar
    actOnStatus mpw status
