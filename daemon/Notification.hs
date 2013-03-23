module Notification where

import Prelude
import Control.Exception
import Data.Text
import Data.Text.Encoding
import Data.Text.Lazy.Builder
import qualified Data.Text.Lazy as LT
import Text.Hamlet
import Text.Blaze.Html.Renderer.String
import Text.Shakespeare.Text
import Network.HTTP.Conduit
import Network.Mail.Mime

import Email
import Model
import Twilio
import Settings

notifyEmail :: Text -> Section -> IO (RequestStatus, Maybe Text)
notifyEmail email section = do
    message <- simpleMail toAddr fromAddr subject text html []
    result <- (try $ mySendmail message)
    case result of
        Left ex ->
            return (Failure, Just $ pack $ show (ex :: SomeException))
        Right _ -> return (Success, Nothing)
    where
        toAddr = Address Nothing email
        fromAddr = noreplyAddr
        subject = LT.toStrict $ LT.init $ toLazyText $ 
            $(textFile "templates/notifications/subject.text") ()
        text = toLazyText $
            $(textFile "templates/notifications/mail.text") ()
        html = LT.pack $ renderHtml $
            $(shamletFile "templates/notifications/mail.hamlet")

notifySms :: Manager
             -> Extra
             -> Text
             -> Section
             -> IO (RequestStatus, Maybe Text)
notifySms manager extra recipient section = do
    result <- try $ sendSms manager extra (encodeUtf8 recipient) message
    case result of
        Left ex ->
            return (Failure, Just $ pack $ show (ex :: SomeException))
        Right err ->
            return (case err of Nothing -> Success; _ -> Failure, err)
    where
        message = encodeUtf8 $ LT.toStrict $ toLazyText $ 
            $(textFile "templates/notifications/sms.text") ()
