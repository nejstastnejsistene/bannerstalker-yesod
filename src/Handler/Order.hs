module Handler.Order where

import Prelude
import Import
import Data.Maybe (isJust, fromJust, catMaybes)
import Data.Text (pack, unpack)
import Text.Printf (printf)

import Stripe

data Order = Order { orderCourseId :: Text
                   , orderCrns :: Maybe [Int]
                   , orderEmail :: Maybe Text
                   , orderPhoneNum :: Maybe Text
                   , orderPhoneCall :: Bool
                   } deriving (Show, Read)

orderKey :: Text
orderKey = "_order"

getStartOrderR :: Handler RepHtml
getStartOrderR = do
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
            setSession orderKey $ pack $ show order
            redirect ChooseCrnsR

getChooseCrnsR :: Handler RepHtml
getChooseCrnsR = do
    mOrder <- getSessionWith orderKey
    case fmap (read . unpack) mOrder of
        Just (Order courseId (Just crns) _ _ _) -> do
            sections <- fmap (map entityVal) $ runDB $
                selectList [SectionCourseId ==. courseId] [Asc SectionCrn]
            defaultLayout $ do
                setTitle "Choose related sections"
                $(widgetFile "choose-crns")
        _ -> deleteSession orderKey >> redirect StartOrderR

postChooseCrnsR :: Handler RepHtml
postChooseCrnsR = do
    (postData, _) <- runRequestBody
    let crns = map (read . unpack . snd) postData
    mOrder <- getSessionWith orderKey
    case fmap (read . unpack) mOrder of
        Just order -> case crns of
            [] -> defaultLayout [whamlet|must choose at least 1 crn|]
            _ -> do
                setSession orderKey $ pack $ show $
                    order { orderCrns = Just crns }
                redirect ContactInfoR
        Nothing -> deleteSession orderKey >> redirect StartOrderR

getContactInfoR :: Handler RepHtml
getContactInfoR = do
    mOrder <- getSessionWith orderKey
    case fmap (read . unpack) mOrder of
        Just (Order _ (Just _) _ _ _) -> do
            user <- fmap (entityVal . fromJust) currentUser
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
    case fmap (read . unpack) mOrder of
        Just order -> do
            setSession orderKey $ pack $ show $
                order { orderEmail = Just email
                      , orderPhoneNum = Just phoneNum
                      , orderPhoneCall = isJust phoneCall }
            redirect ReviewOrderR
        _ -> deleteSession orderKey >> redirect StartOrderR

getReviewOrderR :: Handler RepHtml
getReviewOrderR = do
    mOrder <- getSessionWith orderKey
    case fmap (read . unpack) mOrder of
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
                [] -> defaultLayout $ do
                    setTitle "Review Order"
                    $(widgetFile "review-order")
                _ -> defaultLayout [whamlet|not same course id|]
        _ -> deleteSession orderKey >> redirect StartOrderR

postReviewOrderR :: Handler RepHtml
postReviewOrderR = do
    (price, stripeToken) <- runInputPost $ (,)
        <$> ireq intField "price"
        <*> ireq textField "stripeToken"
    mOrder <- getSessionWith orderKey
    case fmap (read . unpack) mOrder of
        Just (Order _ (Just crns)
                      (Just email)
                      (Just phoneNum)
                      phoneCall) -> do
            Entity userId user <- fmap fromJust currentUser
            manager <- fmap httpManager getYesod
            extra <- getExtra
            eCharge <- liftIO $ makeCharge manager extra stripeToken
                (pack $ show price) $ userEmail user
            case eCharge of
                Left charge -> do
                    deleteSession orderKey
                    mSections <- runDB $ mapM (getBy . UniqueCrn) crns
                    let sectionIds = map entityKey $ catMaybes mSections
                        req = SectionRequest userId email phoneNum phoneCall
                    runDB $ mapM_ (insert . req) sectionIds
                    --sendConfirmation userId name targetLevel charge
                    --setSession upgradeSuccessKey $ T.concat
                    --    [ "Transaction successful!"
                    --    , " We sent you a confirmation email." ]
                    defaultLayout [whamlet|success $#{formatPrice price} #{show charge}|]
                Right err -> do
                    --setSession upgradeErrorKey $ T.concat
                    --    [ errorMessage err
                    --    , " Your card has not been charged."]
                    --redirect ReviewOrderR
                    defaultLayout [whamlet|#{show err}, your card has not been charged, redirect to review payment page|]
        _ -> deleteSession orderKey >> redirect StartOrderR
     

formatPrice :: Int -> Text
formatPrice price =
    pack $ printf "%.2f" $ (/100.0) $ (fromIntegral price :: Float)
