import Prelude
import Yesod.Default.Config     (fromArgs, appExtra)
import Application              (getPersistConfig)
import Settings                 (parseExtra)
import Bannerstalkerd           (bannerstalkerdLoop)
import System.Posix.Daemonize
import Network.HTTP.Conduit

daemon :: CreateDaemon ()
daemon = CreateDaemon {  privilegedAction = return ()
                      ,  program = const startDaemon
                      ,  name = Just "bannerstalkerd"
                      ,  user = Just "bannerstalker"
                      ,  group = Just "bannerstalker"
                      ,  syslogOptions = []
                      ,  pidfileDirectory = Just "/var/run"
                      }

startDaemon :: IO ()
startDaemon = do
    conf <- fromArgs parseExtra
    dbConf <- getPersistConfig conf
    manager <- newManager def
    bannerstalkerdLoop (appExtra conf) dbConf manager


main :: IO ()
--main = serviced daemon
main = startDaemon
