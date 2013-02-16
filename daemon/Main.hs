import Prelude              (IO)
import Yesod.Default.Config (fromArgs)
import Application          (getPersistConfig)
import Settings             (parseExtra)
import Bannerstalkerd       (startDaemon)

main :: IO ()
main = do
    conf <- fromArgs parseExtra
    dbConf <- getPersistConfig conf
    startDaemon dbConf
