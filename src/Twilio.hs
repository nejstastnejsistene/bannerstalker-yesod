-- Adapted from https://github.com/lassoinc/hs-twilio
module Twilio where
import Prelude
import Network.HTTP.Conduit
import Data.Conduit
import Data.Monoid
import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Encoding (decodeUtf8)
import Network.HTTP.Types
import qualified Data.ByteString.UTF8 as U8

import Settings

data TwilioData = TwilioData {
    twilioManager :: Manager,
    twilioRequest :: Request (ResourceT IO)
}

data TwilioCredentials = TwilioCredentials {
    twilioAccount :: ByteString,
    twilioToken :: ByteString
} deriving (Show, Eq, Read)

mkTwilio :: Manager -> TwilioCredentials -> IO TwilioData
mkTwilio manager (TwilioCredentials account token) = do
    req <- parseUrl $ "https://api.twilio.com/2010-04-01/Accounts/" ++
            U8.toString account
    return $ TwilioData manager $ (applyBasicAuth account token) req

twilioReq :: ByteString
             -> [(ByteString, ByteString)]
             -> Bool
             -> TwilioData
             -> IO (Maybe Text)
twilioReq p params post (TwilioData manager request) =
    runResourceT $ do
        response <- httpLbs req'' manager
        case (responseStatus response) of
            Status 201 "Created" -> return Nothing 
            _ -> return $ Just $
                    (toStrict . decodeUtf8) $ responseBody response
        where
            req' = request { path = path request `mappend` p
                           ,  checkStatus = \_ _ -> Nothing }
            ascii = renderSimpleQuery True params
            req'' = if post then urlEncodedBody params req'
                    else req' { queryString = ascii }

sendSms :: Manager -> Extra -> ByteString -> ByteString -> IO (Maybe Text)
sendSms manager extra to body = do
    twilio <- mkTwilio manager credentials
    twilioReq "/SMS/Messages" params True twilio
    where
        account = (encodeUtf8 $ extraTwilioAccount extra) 
        token = (encodeUtf8 $ extraTwilioToken extra) 
        from = (encodeUtf8 $ extraTwilioNumber extra)
        credentials = TwilioCredentials account token
        params = [("To",   to)
                 ,("From", from)
                 ,("Body", body)]

