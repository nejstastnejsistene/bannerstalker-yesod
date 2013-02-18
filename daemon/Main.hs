import Prelude                  (IO)
import Yesod.Default.Config     (fromArgs)
import Application              (getPersistConfig)
import Settings                 (parseExtra)
import Bannerstalkerd           (bannerstalkerd)
import System.Posix.Daemonize   (CreateDaemon, serviced)

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
    bannerstalkerd dbConf


main :: IO ()
--main = serviced daemon
main = startDaemon
