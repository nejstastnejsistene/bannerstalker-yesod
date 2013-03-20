import Prelude
import System.Posix.Daemonize
import Network.HTTP.Conduit
import Yesod.Default.Config

import Application
import Bannerstalkerd
import Settings

type ConfigFiles = (AppConfig DefaultEnv Extra, PersistConfig)

daemon :: CreateDaemon ConfigFiles
daemon = simpleDaemon { privilegedAction = readConfig
                      , program = startDaemon
                      , name = Just "bannerstalkerd"
                      , user = Just "bannerstalker"
                      , group = Just "bannerstalker"
                      }

readConfig :: IO ConfigFiles
readConfig = do
    conf <- getAppConfig Testing
    dbConf <- getPersistConfig conf
    return (conf, dbConf)

startDaemon :: ConfigFiles -> IO ()
startDaemon (conf, dbConf) = do
    manager <- newManager def
    bannerstalkerdLoop (appExtra conf) dbConf manager

main :: IO ()
main = serviced daemon
