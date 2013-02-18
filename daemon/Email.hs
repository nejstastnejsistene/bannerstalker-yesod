import Data.Text
import qualified Data.Text.Lazy as LT
import qualified Data.ByteString.Lazy.Char8 as B
import Network.Mail.Mime
import Text.Hamlet
import Text.Blaze.Html.Renderer.String
import Model

fromAddr :: Address
fromAddr  = Address (Just "Bannerstalker") "admin@bannerstalker.com"

notifyUser :: String -> String -> Section -> IO ()
notifyUser nameStr emailStr section = do
    let name = pack nameStr
        email = pack emailStr
        toAddr = Address (Just name) email
        text = LT.pack $ renderHtml $(shamletFile
                            "templates/mail-notification-text.hamlet")
        html = LT.pack $ renderHtml $(shamletFile
                            "templates/mail-notification-html.hamlet")
    message <- simpleMail toAddr fromAddr "subject" text html []
    out <- renderMail' message
    B.putStrLn out

main :: IO ()
main = do
    let section = Section "semester" 123 "MATH" "303" "Title" "" "" "" Open
        name = "Peter Johnson"
        email = "pajohnson@email.wm.edu"
    notifyUser name email section
