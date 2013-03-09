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

import Email
import Model
import Twilio
import Settings

notifyEmail :: Text -> Section -> Bool -> IO (RequestStatus, Maybe Text)
notifyEmail email section newRequest = do
    message <- simpleMail toAddr fromAddr subject text html []
    result <- (try $ mySendmail message)
    case result of
        Left ex ->
            return (Failure, Just $ pack $ show (ex :: SomeException))
        Right _ -> return (Success, Nothing)
    where
        toAddr = Address Nothing email
        fromAddr = noreplyAddr
        subject = pack $ renderHtml $ case newRequest of
            False -> $(shamletFile
                        "templates/notifications/mail-subj.hamlet")
            True -> $(shamletFile
                        "templates/notifications/mail-subj-new.hamlet")
        text = LT.pack $ renderHtml $ case newRequest of
            False -> $(shamletFile
                        "templates/notifications/mail-text.hamlet")
            True -> $(shamletFile
                        "templates/notifications/mail-text-new.hamlet")
        html = LT.pack $ renderHtml $ case newRequest of
            False -> $(shamletFile
                        "templates/notifications/mail-html.hamlet")
            True -> $(shamletFile
                        "templates/notifications/mail-html-new.hamlet")

notifySms :: Manager
             -> Extra
             -> Text
             -> Section
             -> Bool
             -> IO (RequestStatus, Maybe Text)
notifySms manager extra recipient section newRequest = do
    twilio <- mkTwilio manager credentials
    err <- sendSms twilio number (encodeUtf8 recipient) message
    return (case err of Nothing -> Success; _ -> Failure, err)
    where
        account = (encodeUtf8 $ extraTwilioAccount extra) 
        token = (encodeUtf8 $ extraTwilioToken extra) 
        number = (encodeUtf8 $ extraTwilioNumber extra)
        credentials = TwilioCredentials account token
        message = encodeUtf8 $ pack $ renderHtml $ case newRequest of
            False -> $(shamletFile "templates/notifications/sms.hamlet")
            True -> $(shamletFile "templates/notifications/sms-new.hamlet")
