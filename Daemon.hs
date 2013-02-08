import Control.Monad
import System.Posix.Daemonize

daemon = CreateDaemon {  privilegedAction = return ()
                      ,  program = bannerstalkerd
                      ,  name = Just "bannerstalkerd"
                      ,  user = Just "bannerstalker"
                      ,  group = Just "bannerstalker"
                      ,  syslogOptions = []
                      ,  pidfileDirectory = Just "/var/run"
                      }

bannerstalkerd = const $ forever $ return ()

main = serviced daemon
