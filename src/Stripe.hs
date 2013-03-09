module Stripe where

import Prelude hiding (concat)
import Control.Applicative
import Control.Monad
import Data.Aeson hiding (Error)
import Data.Conduit
import Data.Text (Text)
import Data.Text.Encoding
import Network.HTTP.Conduit
import Network.HTTP.Types

import Settings

data Charge = Charge { chargeId :: Text
                     , chargeObject :: Text
                     , chargeLivemode :: Bool
                     , chargeAmount :: Int
                     , chargeCard :: Card
                     , chargeCreated :: Int
                     , chargeCurrency :: Text
                     , chargeFee :: Int
                     , chargeFeeDetails :: [Fee]
                     , chargePaid :: Bool
                     , chargeRefunded :: Bool
                     , chargeAmountRefunded :: Maybe Int
                     , chargeCustomer :: Maybe Text
                     , chargeDescription :: Maybe Text
                     , chargeDispute :: Maybe [Dispute]
                     , chargeFailureMessage :: Maybe Text
                     , chargeInvoice :: Maybe Text
                     } deriving Show

data Card = Card { cardObject :: Text
                 , cardExpMonth :: Int
                 , cardExpYear :: Int
                 , cardFingerprint :: Text
                 , cardLast4 :: Text
                 , cardType :: Text
                 , cardAddressCity :: Maybe Text
                 , cardAddressCountry :: Maybe Text
                 , cardAddressLine1 :: Maybe Text
                 , cardAddressLine1Check :: Maybe Text
                 , cardAddressLine2 :: Maybe Text
                 , cardAddressState :: Maybe Text
                 , cardAddressZip :: Maybe Text
                 , cardAddressZipCheck :: Maybe Text
                 , cardCountry :: Maybe Text
                 , cardCvcCheck :: Maybe Text
                 , cardName :: Maybe Text
                 } deriving Show

data Fee = Fee { feeAmount :: Int
               , feeCurrency :: Text
               , feeType :: Text
               , feeAmountRefunded :: Maybe Int
               , feeApplication :: Maybe Text
               , feeDescription :: Maybe Text
               } deriving Show

data Dispute = Dispute { disputeObject :: Text
                       , disputeLivemode :: Bool
                       , disputeAmount :: Int
                       , disputeCharge :: Text
                       , disputeCreated :: Text
                       , disputeCurrency :: Text
                       , disputeEvidenceDueBy :: Text
                       , disputeReason :: Text
                       , disputeStatus :: Text
                       , disputeEvidence :: Maybe Text 
                       } deriving Show

data OuterError = OuterError Error deriving Show
data Error = Error { errorType :: Text
                   , errorMessage :: Text
                   , errorCode :: Maybe Text
                   , errorCharge :: Maybe Text
                   } deriving Show

instance FromJSON Charge where
    parseJSON (Object o) = Charge
        <$> o .: "id"
        <*> o .: "object"
        <*> o .: "livemode"
        <*> o .: "amount"
        <*> o .: "card"
        <*> o .: "created"
        <*> o .: "currency"
        <*> o .: "fee"
        <*> o .: "fee_details"
        <*> o .: "paid"
        <*> o .: "refunded"
        <*> o .: "amount_refunded"
        <*> o .: "customer"
        <*> o .: "description"
        <*> o .: "dispute"
        <*> o .: "failure_message"
        <*> o .: "invoice"
    parseJSON _ = mzero

instance FromJSON Card where
    parseJSON (Object o) = Card
        <$> o .: "object"
        <*> o .: "exp_month"
        <*> o .: "exp_year"
        <*> o .: "fingerprint"
        <*> o .: "last4"
        <*> o .: "type"
        <*> o .: "address_city"
        <*> o .: "address_country"
        <*> o .: "address_line1"
        <*> o .: "address_line1_check"
        <*> o .: "address_line2"
        <*> o .: "address_state"
        <*> o .: "address_zip"
        <*> o .: "address_zip_check"
        <*> o .: "country"
        <*> o .: "cvc_check"
        <*> o .: "name"
    parseJSON _ = mzero

instance FromJSON Fee where
    parseJSON (Object o) = Fee
        <$> o .: "amount"
        <*> o .: "currency"
        <*> o .: "type"
        <*> o .: "amount_refunded"
        <*> o .: "application"
        <*> o .: "description"
    parseJSON _ = mzero

instance FromJSON Dispute where
    parseJSON (Object o) = Dispute
        <$> o .: "object"
        <*> o .: "livemode"
        <*> o .: "amount"
        <*> o .: "charge"
        <*> o .: "created"
        <*> o .: "currency"
        <*> o .: "evidence_due_by"
        <*> o .: "reason"
        <*> o .: "status"
        <*> o .: "evidence"
    parseJSON _ = mzero

instance FromJSON OuterError where
    parseJSON (Object o) = OuterError <$> o .: "error"
    parseJSON _ = mzero

instance FromJSON Error where
    parseJSON (Object o) = Error
        <$> o .: "type"
        <*> o .: "message"
        <*> o .: "code"
        <*> o .: "charge"
    parseJSON _ = mzero

makeCharge :: Manager
              -> Extra
              -> Text
              -> Text
              -> Text
              -> IO (Either Charge Error)
makeCharge manager extra stripeToken amountCents email = do
    req <- parseUrl url
    let req' = urlEncodedBody params $
            applyBasicAuth publicKey "" $
            req { checkStatus = \_ _ -> Nothing }
    response <- runResourceT $ httpLbs req' manager
    let body = responseBody response
    case (responseStatus response) of
        Status 200 _ -> do
            case decode body of
                Just charge -> return $ Left (charge :: Charge)
                Nothing -> error $ show body
        _ -> do
            case decode body of
                Just (OuterError err) -> return $ Right err
                Nothing -> error $ show body
    where
        url = "https://api.stripe.com/v1/charges"
        publicKey = encodeUtf8 $ extraStripeSecretKey extra
        params = [ ("amount", encodeUtf8 amountCents)
                 , ("currency", "usd")
                 , ("card", encodeUtf8 stripeToken)
                 , ("description", encodeUtf8 email)
                 ]
