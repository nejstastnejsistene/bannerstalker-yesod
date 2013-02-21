-- Adapted from https://github.com/lassoinc/hs-twilio
{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
module Twil where
import Prelude
import Network.HTTP.Conduit
import Data.Conduit
import Data.Monoid
import Data.ByteString (ByteString)
import Network.HTTP.Types
import qualified Data.ByteString.UTF8 as U8

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
             -> IO ()
twilioReq p params post (TwilioData manager request) =
    runResourceT $ do
        response <- httpLbs req'' manager
        case (responseStatus response) of
            Status 201 "Created" -> return ()
            _                    -> error $ show $ responseBody response
        where
            req' = request { path = path request `mappend` p }
            ascii = renderSimpleQuery True params
            req'' = if post then urlEncodedBody params req'
                    else req' { queryString = ascii }

sendSms :: TwilioData
           -> ByteString -- ^ From
           -> ByteString -- ^ To
           -> ByteString -- ^ Body
           -> IO ()
sendSms twilioData from to body = do
    twilioReq "/SMS/Messages" params True twilioData
    where
        params = [("To",   to)
                 ,("From", from)
                 ,("Body", body)]

