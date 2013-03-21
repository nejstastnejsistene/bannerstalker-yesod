module Handler.Order where

import Prelude
import Import
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import Network.Mail.Mime
import Text.Blaze.Html.Renderer.String
import Text.Hamlet
import Text.Printf (printf)

import Email
import Stripe

data Order = Order { orderCourseId :: Text
                   , orderCrns :: Maybe [Int]
                   , orderEmail :: Maybe Text
                   , orderPhoneNum :: Maybe Text
                   , orderPhoneCall :: Bool
                   } deriving (Show, Read)

orderKey, errorKey, successKey :: Text
orderKey = "_order"
errorKey = "_orderError"
successKey = "_orderSuccess"

getStartOrderR :: Handler RepHtml
getStartOrderR = do
    mErrorMessage <- consumeSession errorKey
    mSuccessMessage <- consumeSession successKey
    defaultLayout $ do
        setTitle "Stalk a CRN"
        $(widgetFile "start-order")

postStartOrderR :: Handler RepHtml
postStartOrderR = do
    crn <- runInputPost $ ireq intField "crn"
    mSection <- runDB $ getBy $ UniqueCrn crn
    case mSection of
        Nothing -> do
            redirect StartOrderR
        Just (Entity _ section) -> do
            let order =  Order (sectionCourseId section)
                            (Just [crn]) Nothing Nothing False
            setSession orderKey $ T.pack $ show order
            redirect ChooseCrnsR

getChooseCrnsR :: Handler RepHtml
getChooseCrnsR = do
    mOrder <- getSessionWith orderKey
    case fmap (read . T.unpack) mOrder of
        Just (Order courseId (Just crns) _ _ _) -> do
            sections <- fmap (map entityVal) $ runDB $
                selectList [SectionCourseId ==. courseId] [Asc SectionCrn]
            mErrorMessage <- consumeSession errorKey
            defaultLayout $ do
                setTitle "Choose related sections"
                $(widgetFile "choose-crns")
        _ -> deleteSession orderKey >> redirect StartOrderR

postChooseCrnsR :: Handler RepHtml
postChooseCrnsR = do
    (postData, _) <- runRequestBody
    let crns = map (read . T.unpack . snd) postData
    mOrder <- getSessionWith orderKey
    case fmap (read . T.unpack) mOrder of
        Just order -> case crns of
            [] -> do
                setSession errorKey "You must select at least 1 CRN."
                redirect ChooseCrnsR
            _ -> do
                setSession orderKey $ T.pack $ show $
                    order { orderCrns = Just crns }
                redirect ContactInfoR
        Nothing -> deleteSession orderKey >> redirect StartOrderR

getContactInfoR :: Handler RepHtml
getContactInfoR = do
    mOrder <- getSessionWith orderKey
    case fmap (read . T.unpack) mOrder of
        Just (Order _ (Just _) _ _ _) -> do
            user <- fmap (entityVal . fromJust) currentUser
            mErrorMessage <- consumeSession errorKey
            defaultLayout $ do
                setTitle "Contact information"
                $(widgetFile "contact-info")
        _ -> deleteSession orderKey >> redirect StartOrderR

postContactInfoR :: Handler RepHtml
postContactInfoR = do
    (email, phoneNum, phoneCall) <- runInputPost $ (,,)
        <$> ireq emailField "email"
        <*> ireq textField "phoneNum"
        <*> iopt boolField "phoneCall"
    mOrder <- getSessionWith orderKey
    case fmap (read . T.unpack) mOrder of
        Just order -> do
            setSession orderKey $ T.pack $ show $
                order { orderEmail = Just email
                      , orderPhoneNum = Just phoneNum
                      , orderPhoneCall = isJust phoneCall }
            redirect ReviewOrderR
        _ -> deleteSession orderKey >> redirect StartOrderR

getReviewOrderR :: Handler RepHtml
getReviewOrderR = do
    mOrder <- getSessionWith orderKey
    case fmap (read . T.unpack) mOrder of
        Just (Order courseId
                    (Just crns)
                    (Just email)
                    (Just phoneNum)
                    phoneCall) -> do
            mSections <- runDB $ mapM (getBy . UniqueCrn) crns
            let sections = map entityVal $ catMaybes mSections
                sectionsPrice = 500 + 100 * (length sections - 1)
                additionalPrice = if phoneCall then 300 else 0
                price = sectionsPrice + additionalPrice
            case dropWhile (==courseId) $ map sectionCourseId sections of
                [] -> do
                    mErrorMessage <- consumeSession errorKey
                    defaultLayout $ do
                        setTitle "Review Order"
                        $(widgetFile "review-order")
                _ -> do
                    setSession errorKey 
                        "You can purchase at most one course ID per order."
                    redirect ChooseCrnsR
        _ -> deleteSession orderKey >> redirect StartOrderR

postReviewOrderR :: Handler RepHtml
postReviewOrderR = do
    (price, stripeToken) <- runInputPost $ (,)
        <$> ireq intField "price"
        <*> ireq textField "stripeToken"
    mOrder <- getSessionWith orderKey
    case fmap (read . T.unpack) mOrder of
        Just (Order _ (Just crns)
                      (Just email)
                      (Just phoneNum)
                      phoneCall) -> do
            Entity userId user <- fmap fromJust currentUser
            manager <- fmap httpManager getYesod
            extra <- getExtra
            eCharge <- liftIO $ makeCharge manager extra stripeToken
                (T.pack $ show (price :: Int)) $ userEmail user
            case eCharge of
                Left charge -> do
                    deleteSession orderKey
                    mSections <- runDB $ mapM (getBy . UniqueCrn) crns
                    let sectionIds = map entityKey $ catMaybes mSections
                        req = SectionRequest userId email phoneNum phoneCall
                    runDB $ mapM_ (insert . req) sectionIds
                    sendConfirmation email charge
                    setSession successKey $ T.concat
                        [ "Transaction successful!"
                        , " We sent you a confirmation email." ]
                    redirect StartOrderR
                Right err -> do
                    setSession errorKey $ T.concat
                        [ errorMessage err
                        , ". Your card has not been charged."]
                    redirect ReviewOrderR
        _ -> deleteSession orderKey >> redirect StartOrderR
     
sendConfirmation :: Text -> Charge -> Handler ()
sendConfirmation email charge = do
    let to = Address Nothing email
        from = noreplyAddr
        subject = "Bannerstalker transaction confirmation"
        text = LT.pack $ renderHtml
            $(shamletFile "templates/transaction-confirmation-text.hamlet")
        html = LT.pack $ renderHtml
            $(shamletFile "templates/transaction-confirmation-html.hamlet")
    liftIO $ simpleMail to from subject text html [] >>= mySendmail

formatPrice :: Int -> Text
formatPrice price =
    T.pack $ printf "%.2f" $ (/100.0) $ (fromIntegral price :: Float)
