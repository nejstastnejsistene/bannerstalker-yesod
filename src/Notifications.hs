module Notifications where
import Prelude
import Yesod.Default.Config
import Settings
import Data.Text.Encoding

import Control.Exception
import Data.Text
import qualified Data.Text.Lazy as LT
import Database.Persist
import Text.Hamlet
import Text.Blaze.Html.Renderer.String
import Network.HTTP.Conduit
import Network.Mail.Mime

import Model
import Twilio

fromAddr :: Address
fromAddr  = Address (Just "Bannerstalker") "admin@bannerstalker.com"

notifyEmail :: Text -> Section -> IO (RequestStatus, Maybe Text)
notifyEmail email section= do
    message <- simpleMail toAddr fromAddr "subject" text html []
    result <- (try $ renderSendMail message)
    case result of
        Left ex ->
            return (Failure, Just $ pack $ show (ex :: SomeException))
        Right _ -> return (Success, Nothing)
    where
        toAddr = Address Nothing email
        text = LT.pack $ renderHtml
                $(shamletFile "templates/mail-notification-text.hamlet")
        html = LT.pack $ renderHtml
                $(shamletFile "templates/mail-notification-html.hamlet")

notifySms :: Manager
             -> Extra
             -> Text
             -> Section
             -> IO (RequestStatus, Maybe Text)
notifySms manager extra recipient section = do
    twilio <- mkTwilio manager credentials
    err <- sendSms twilio number (encodeUtf8 recipient) "Notification!"
    return (case err of Nothing -> Success; _ -> Failure, err)
    where
        account = (encodeUtf8 $ extraTwilioAccount extra) 
        token = (encodeUtf8 $ extraTwilioToken extra) 
        number = (encodeUtf8 $ extraTwilioNumber extra)
        credentials = TwilioCredentials account token

main :: IO ()
main = do
    extra <- fmap appExtra $ fromArgs parseExtra
    manager <- newManager def
    let creds = TwilioCredentials
                    (encodeUtf8 $ extraTwilioAccount extra) 
                    (encodeUtf8 $ extraTwilioToken extra) 
    twilio <- mkTwilio manager creds
    err <- sendSms twilio "+15714510230" "+19034754114" "It works now!"
    case err of
        Nothing -> return ()
        Just body -> putStrLn $ show body

