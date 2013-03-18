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
        subject = LT.toStrict $ toLazyText $ (case newRequest of
            False -> $(textFile "templates/notifications/subject.text")
            True -> $(textFile "templates/notifications/subject-new.text")
            ) ()
        text = toLazyText $ (case newRequest of
            False -> $(textFile "templates/notifications/mail.text")
            True -> $(textFile "templates/notifications/mail-new.text")
            ) ()
        html = LT.pack $ renderHtml $ case newRequest of
            False -> $(shamletFile
                        "templates/notifications/mail.hamlet")
            True -> $(shamletFile
                        "templates/notifications/mail-new.hamlet")

notifySms :: Manager
             -> Extra
             -> Text
             -> Section
             -> Bool
             -> IO (RequestStatus, Maybe Text)
notifySms manager extra recipient section newRequest = do
    err <- sendSms manager extra (encodeUtf8 recipient) message
    return (case err of Nothing -> Success; _ -> Failure, err)
    where
        message = encodeUtf8 $ LT.toStrict $ toLazyText $ 
            (case newRequest of
                False-> $(textFile "templates/notifications/sms.text")
                True -> $(textFile "templates/notifications/sms-new.text")
            ) ()
