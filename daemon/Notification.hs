module Notification where
import Prelude
import Control.Exception
import Data.Text
import Data.Text.Encoding
import qualified Data.Text.Lazy as LT
import Text.Hamlet
import Text.Blaze.Html.Renderer.String
import Network.HTTP.Conduit
import Network.Mail.Mime

import Model
import Twilio
import Settings

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
                $(shamletFile "templates/notification-mail-text.hamlet")
        html = LT.pack $ renderHtml
                $(shamletFile "templates/notification-mail-html.hamlet")

notifySms :: Manager
             -> Extra
             -> Text
             -> Section
             -> IO (RequestStatus, Maybe Text)
notifySms manager extra recipient section = do
    twilio <- mkTwilio manager credentials
    err <- sendSms twilio number (encodeUtf8 recipient) message
    return (case err of Nothing -> Success; _ -> Failure, err)
    where
        account = (encodeUtf8 $ extraTwilioAccount extra) 
        token = (encodeUtf8 $ extraTwilioToken extra) 
        number = (encodeUtf8 $ extraTwilioNumber extra)
        credentials = TwilioCredentials account token
        message = encodeUtf8 $ pack $ renderHtml 
            $(shamletFile "templates/notification-sms.hamlet")
