import Prelude              (IO)
import Yesod.Default.Config
import Yesod.Default.Main   (defaultMain)
import Application          (getAppConfig, makeApplication)

main :: IO ()
main = defaultMain (getAppConfig Development) makeApplication
