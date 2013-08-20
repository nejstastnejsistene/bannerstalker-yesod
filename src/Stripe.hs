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
                     , chargeCreated :: Int
                     , chargeLivemode :: Bool
                     , chargePaid :: Bool
                     , chargeAmount :: Int
                     , chargeCurrency :: Text
                     , chargeRefunded :: Bool
                     , chargeCard :: Card
                     , chargeCapture :: Maybe Bool
                     , chargeBalanceTransaction :: Maybe Text
                     , chargeFailureMessage :: Maybe Text
                     , chargeFailureCode :: Maybe Text
                     , chargeAmountRefunded :: Maybe Int
                     , chargeCustomer :: Maybe Text
                     , chargeInvoice :: Maybe Text
                     , chargeDescription :: Maybe Text
                     , chargeDispute :: Maybe [Dispute]
                     , chargeFee :: Int
                     , chargeFeeDetails :: [Fee]
                     } deriving Show

data Card = Card { cardId :: Text
                 , cardObject :: Text
                 , cardLast4 :: Text
                 , cardType :: Text
                 , cardExpMonth :: Int
                 , cardExpYear :: Int
                 , cardFingerprint :: Text
                 , cardCustomer :: Maybe Text
                 , cardCountry :: Maybe Text
                 , cardName :: Maybe Text
                 , cardAddressCity :: Maybe Text
                 , cardAddressCountry :: Maybe Text
                 , cardAddressLine1 :: Maybe Text
                 , cardAddressLine2 :: Maybe Text
                 , cardAddressState :: Maybe Text
                 , cardAddressZip :: Maybe Text
                 , cardCvcCheck :: Maybe Text
                 , cardAddressLine1Check :: Maybe Text
                 , cardAddressZipCheck :: Maybe Text
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
        <*> o .: "created"
        <*> o .: "livemode"
        <*> o .: "paid"
        <*> o .: "amount"
        <*> o .: "currency"
        <*> o .: "refunded"
        <*> o .: "card"
        <*> o .: "captured"
        <*> o .: "balance_transaction"
        <*> o .: "failure_message"
        <*> o .: "failure_code"
        <*> o .: "amount_refunded"
        <*> o .: "customer"
        <*> o .: "invoice"
        <*> o .: "description"
        <*> o .: "dispute"
        <*> o .: "fee"
        <*> o .: "fee_details"
    parseJSON _ = mzero

instance FromJSON Card where
    parseJSON (Object o) = Card
        <$> o .: "id"
        <*> o .: "object"
        <*> o .: "last4"
        <*> o .: "type"
        <*> o .: "exp_month"
        <*> o .: "exp_year"
        <*> o .: "fingerprint"
        <*> o .: "customer"
        <*> o .: "country"
        <*> o .: "name"
        <*> o .: "address_city"
        <*> o .: "address_country"
        <*> o .: "address_line1"
        <*> o .: "address_line2"
        <*> o .: "address_state"
        <*> o .: "address_zip"
        <*> o .: "cvc_check"
        <*> o .: "address_line1_check"
        <*> o .: "address_zip_check"
    parseJSON _ = mzero

instance FromJSON Fee where
    parseJSON (Object o) = Fee
        <$> o .: "amount"
        <*> o .: "currency"
        <*> o .: "type"
        <*> o .:? "amount_refunded"
        <*> o .:? "description"
        <*> o .:? "application"
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
