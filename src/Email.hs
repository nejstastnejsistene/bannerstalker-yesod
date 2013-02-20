module Email where

import Prelude
import Data.ByteString.Lazy.Char8
import Data.Text
import qualified Data.Text.Lazy as LT
import Network.Mail.Mime
import Text.Hamlet
import Text.Blaze.Html.Renderer.String
import Model

fromAddr :: Address
fromAddr  = Address (Just "Bannerstalker") "admin@bannerstalker.com"

createEmail :: Text -> Section -> IO ByteString 
createEmail email section = do
    let toAddr = Address Nothing email
        text = LT.pack $ renderHtml $(shamletFile
                            "templates/mail-notification-text.hamlet")
        html = LT.pack $ renderHtml $(shamletFile
                            "templates/mail-notification-html.hamlet")
    message <- simpleMail toAddr fromAddr "subject" text html []
    renderMail' message
    --renderSendMail message

main :: IO ()
main = do
    let section = Section "semester" 123 "MATH" "303" "Title" "" "" "" Open
        name = "Peter Johnson"
        email = "pajohnson@email.wm.edu"
    mesg <- createEmail email section
    Data.ByteString.Lazy.Char8.putStrLn mesg
