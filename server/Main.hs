import Prelude              (IO)
import Yesod.Default.Config
import Yesod.Default.Main   (defaultMain)
import Application          (getAppConfig, makeApplication)
import Settings             (parseExtra)

main :: IO ()
main = defaultMain (fromArgs parseExtra) makeApplication
