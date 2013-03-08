module Stripe where

import Prelude
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Conduit
import Data.Text (Text)
import Data.Text.Encoding
import Network.HTTP.Conduit
import Network.HTTP.Types

import Settings

makeCharge :: Manager -> Extra -> Text -> Text -> Text -> IO (Maybe Text)
makeCharge manager extra stripeToken amountCents email = do
    req <- parseUrl url
    let req' = urlEncodedBody params $
            applyBasicAuth publicKey "" $
            req { checkStatus = \_ _ -> Nothing }
    runResourceT $ do
        response <- httpLbs req' manager
        let body = decodeUtf8 $
                B.concat $ BL.toChunks $ responseBody response
        case (responseStatus response) of
            Status 200 _ -> return $ Just body
            _ -> return $ Just body
    where
        url = "https://api.stripe.com/v1/charges"
        publicKey = encodeUtf8 $ extraStripeSecretKey extra
        params = [ ("amount", encodeUtf8 amountCents)
                 , ("currency", "usd")
                 , ("card", encodeUtf8 stripeToken)
                 , ("description", encodeUtf8 email)
                 ]
