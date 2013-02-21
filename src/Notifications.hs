import Prelude
import Yesod.Default.Config
import Settings
import Data.Text.Encoding

import Network.HTTP.Conduit

import Twilio
import Model

notifySms :: Manager -> IO ()
notifySms manager = return ()

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

