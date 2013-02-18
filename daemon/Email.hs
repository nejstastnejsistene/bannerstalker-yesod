--import Data.Text (Text)
import GHC.IO.Handle
import System.Exit
import System.Process

import Data.Text
import Network.Mail.Mime
import qualified Data.ByteString.Lazy.Char8 as B

me = Address (Just "Peter Johnson") "pajohnson@email.wm.edu"
admin = Address (Just "Bannerstalker") "admin@bannerstalker.com"

_text = "ahoj!"
_html = "<html><body><p>ahoj!</p></body></html>"

createMessage :: IO ()
createMessage = do
    message <- simpleMail me admin "subject" _text _html []
    out <- renderMail' message
    B.putStrLn out

main :: IO ()
main = createMessage
