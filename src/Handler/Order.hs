module Handler.Order where

import Prelude
import Import
import Data.Char
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import Data.Time
import Database.Persist.GenericSql
import Network.Mail.Mime
import Text.Blaze.Html.Renderer.String
import Text.Hamlet
import Text.Printf (printf)
import System.Locale

import Email
import Stripe
import Handler.Auth

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

getAccountR :: Handler RepHtml
getAccountR = do
    let sql = "SELECT ??, ?? \
              \FROM \"user\", section, section_request \
              \WHERE \"user\".id = ? \
                \AND section_request.user_id = ? \
                \AND section_request.section_id = section.id \
                \AND section_request.active = true \
              \ORDER BY section.course_id ASC"
    Entity userId _ <- fmap fromJust currentUser
    sqlResult <- runDB $ rawSql sql [ toPersistValue userId
                                    , toPersistValue userId ]
    let sectionResults = [(s, r) | (Entity _ s, Entity r _) <- sqlResult]
    courseListLogs <- fmap (map entityVal) $ runDB $ selectList
        [] [Desc CourseListLogTimestamp, LimitTo 1]
    mLastChecked <- case courseListLogs of
        [x] -> do
            tz <- liftIO $ getCurrentTimeZone
            let t = utcToLocalTime tz $ courseListLogTimestamp x
            return $ Just $ formatTime defaultTimeLocale "%D %X" t
        _ -> return Nothing
    mErrorMessage <- consumeSession errorKey
    mSuccessMessage <- consumeSession successKey
    defaultLayout $ do
        setTitle "Account"
        $(widgetFile "account")

getAccountInfoR :: Handler RepHtml
getAccountInfoR = do
    Entity _ user <- fmap fromJust currentUser
    mErrorMessage <- consumeSession errorKey
    mSuccessMessage <- consumeSession successKey
    defaultLayout $ do
        setTitle "Account info"
        $(widgetFile "account-info")

postAccountInfoR :: Handler RepHtml
postAccountInfoR = do
    result <- runInputPost $ (,,,)
        <$> iopt emailField "email"
        <*> iopt textField "phoneNum"
        <*> iopt passwordField "password"
        <*> iopt passwordField "confirm"
    Entity userId user <- fmap fromJust currentUser
    case result of
        (Just email, Just phoneNum, _, _) -> do
            mUser <- runDB $ getBy $ UniqueEmail email
            case (mUser, email ==  userEmail user) of
                (Just _, False) -> do
                    setSession errorKey
                        "That email address is already in use."
                    redirect AccountInfoR
                _ -> case validatePhoneNum phoneNum of
                    Nothing -> do
                        setSession errorKey
                            "Please enter a valid phone number."
                        redirect AccountInfoR
                    Just validPhoneNum -> do
                        runDB $ update userId
                            [ UserEmail =. email
                            , UserPhoneNum =. validPhoneNum ]
                        setSession successKey "Your account \
                            \information was updated."
                        redirect AccountInfoR
        (_, _, Just passwd, Just confirm) -> if passwd == confirm
            then do
                changePassword userId passwd
                setSession successKey
                    "Your password has been updated."
                redirect AccountInfoR
            else do
                setSession errorKey passwordMismatch
                redirect AccountInfoR
        _ -> do
            setSession errorKey "Form error. Please try again."
            redirect AccountInfoR
    

getViewRequestR :: SectionRequestId -> Handler RepHtml
getViewRequestR reqId = do
    userId <- fmap (entityKey . fromJust) currentUser
    reqs <- runDB $ selectList [ SectionRequestId ==. reqId
                               , SectionRequestUserId ==. userId
                               , SectionRequestActive ==. True] []
    case reqs of
        [Entity _ req] -> do
            section <- fmap fromJust $
                runDB $ get $ sectionRequestSectionId req
            let sql = "SELECT ?? \
                      \FROM notification_log, section \
                      \WHERE notification_log.user_id = ? \
                         \AND notification_log.crn = section.crn \
                         \AND section.id = ? \
                      \ORDER BY notification_log.timestamp DESC \
                      \LIMIT 8"
            notifications <- fmap (map entityVal) $ runDB $ rawSql sql
                [ toPersistValue $ userId
                , toPersistValue $ sectionRequestSectionId req ]
            tz <- liftIO $ getCurrentTimeZone
            let localTime = utcToLocalTime tz
            defaultLayout $ do
                setTitle "View request"
                $(widgetFile "view-request")
        _ -> redirect AccountR

postRemoveRequestR :: SectionRequestId -> Handler RepHtml
postRemoveRequestR reqId = do
    (gotIn, _) <- runInputPost $ (,)
        <$> ireq boolField "gotIn"
        <*> ireq boolField "confirm"
    _ <- runDB $ insert $ Feedback reqId gotIn
    userId <- fmap (entityKey . fromJust) currentUser
    reqs <- runDB $ selectList [ SectionRequestId ==. reqId
                               , SectionRequestUserId ==. userId
                               , SectionRequestActive ==. True] []
    case reqs of
        [_] -> do
            runDB $ update reqId [SectionRequestActive =. False]
            setSession successKey "Your CRN was successfully removed."
            redirect AccountR
        _ -> redirect AccountR

postStartOrderR :: Handler RepHtml
postStartOrderR = do
    crn <- runInputPost $ ireq intField "crn"
    mSection <- runDB $ getBy $ UniqueCrn crn
    case mSection of
        Nothing -> do
            setSession errorKey "That CRN doesn't exist!"
            redirect AccountR
        Just (Entity _ section) -> do
            let order = Order (sectionCourseId section)
                            (Just [crn]) Nothing Nothing False
            setSession orderKey $ T.pack $ show order
            redirect ChooseCrnsR

getChooseCrnsR :: Handler RepHtml
getChooseCrnsR = do
    mOrder <- getSessionWith orderKey
    case fmap (read . T.unpack) mOrder of
        Just (Order courseId (Just crns) _ _ _) -> do
            let sql = "SELECT ?? \
                      \FROM section \
                      \WHERE section.course_id SIMILAR TO ? \
                      \ORDER BY section.crn ASC"
            sections <- fmap (map entityVal) $ runDB $
                rawSql sql [toPersistValue $ canonicalizeCourseId courseId]
            mErrorMessage <- consumeSession errorKey
            defaultLayout $ do
                setTitle "Choose related sections"
                $(widgetFile "choose-crns")
        _ -> deleteSession orderKey >> redirect AccountR

-- For example: "PHYS %101%"
canonicalizeCourseId :: Text -> Text
canonicalizeCourseId courseId =
    T.concat [subj, " _?", strippedNum, "_?"]
    where
        subj:num:_ = T.words courseId
        strippedNum = T.dropAround (fmap not isDigit) num

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
        Nothing -> deleteSession orderKey >> redirect AccountR

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
        Just order -> case validatePhoneNum phoneNum of
            Nothing -> do
                setSession errorKey
                    "Please enter a valid US phone number."
                redirect ContactInfoR
            mPhoneNum -> do
                setSession orderKey $ T.pack $ show $
                    order { orderEmail = Just email
                          , orderPhoneNum = mPhoneNum
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
                --additionalPrice = if phoneCall then 300 else 0
                price = offsetFees sectionsPrice
                fee = price - sectionsPrice
                cId = canonicalizeCourseId courseId  
                courseIds = map
                    (canonicalizeCourseId . sectionCourseId) sections
            case dropWhile (==cId) $ courseIds of
                [] -> do
                    extra <- getExtra
                    mErrorMessage <- consumeSession errorKey
                    defaultLayout $ do
                        setTitle "Review Order"
                        $(widgetFile "review-order")
                _ -> do
                    setSession errorKey 
                        "You can purchase at most one course ID per order."
                    redirect ChooseCrnsR
        _ -> deleteSession orderKey >> redirect AccountR
    where
        offsetFees :: Int -> Int
        offsetFees p =
            let cost = fromIntegral (p + 30) / 0.971 :: Double
            in ceiling cost

postReviewOrderR :: Handler RepHtml
postReviewOrderR = do
    (price, stripeToken) <- runInputPost $ (,)
        <$> ireq intField "price"
        <*> ireq textField "stripeToken"
    mOrder <- getSessionWith orderKey
    case fmap (read . T.unpack) mOrder of
        Just order@(Order _ (Just crns)
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
                        req = SectionRequest userId
                            email phoneNum phoneCall True True
                    runDB $ mapM_ (insert . req) sectionIds
                    sendConfirmation email order charge
                    setSession successKey $ T.concat
                        [ "Transaction successful!"
                        , " We sent you a confirmation email as well"
                        , " as notifications for each CRN you just"
                        , " ordered."]
                    redirect AccountR
                Right err -> do
                    setSession errorKey $ T.concat
                        [ errorMessage err
                        , ". Your card has not been charged."]
                    redirect ReviewOrderR
        _ -> deleteSession orderKey >> redirect AccountR
     
sendConfirmation :: Text -> Order -> Charge -> Handler ()
sendConfirmation email order charge = do
    mSections <- runDB $ mapM (getBy . UniqueCrn) $
        fromJust $ orderCrns order
    let sections = map entityVal $ catMaybes mSections
        to = Address Nothing email
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
