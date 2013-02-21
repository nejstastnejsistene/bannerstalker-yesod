import Prelude
import Yesod.Default.Config
import Settings
import Data.Text.Encoding

import Network.HTTP.Conduit

import Twilio

main :: IO ()
main = do
    extra <- fmap appExtra $ fromArgs parseExtra
    manager <- newManager def
    let creds = TwilioCredentials
                    (encodeUtf8 $ extraTwilioAccount extra) 
                    (encodeUtf8 $ extraTwilioToken extra) 
    twilio <- mkTwilio manager creds
    sendSms twilio "+15714510230" "+17034754114" "It works now!"
