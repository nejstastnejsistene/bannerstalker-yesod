module Notifications where
import Prelude
import Yesod.Default.Config
import Settings
import Data.Text.Encoding

import Network.HTTP.Conduit

import Database.Persist
import Data.Text

import Twilio
import Model

notifySms :: Manager -> Extra -> Text -> Section -> IO (Maybe Text)
notifySms manager extra recipient section = do
    twilio <- mkTwilio manager credentials
    sendSms twilio number (encodeUtf8 recipient) "Notification!"
    where
        account = (encodeUtf8 $ extraTwilioAccount extra) 
        token = (encodeUtf8 $ extraTwilioToken extra) 
        number = (encodeUtf8 $ extraTwilioNumber extra)
        credentials = TwilioCredentials account token
        

notifyEmail :: IO ()
notifyEmail = return ()

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

