module Handler.NewOrder where

import Import
import Control.Monad (when)
import Data.Char (isDigit)
import Data.Either (partitionEithers)
import Data.Maybe (fromJust, isNothing)
import qualified Data.List as L
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import Data.Text.Lazy.Builder (toLazyText)
import Database.Persist.GenericSql
import Network.Mail.Mime
import Text.Hamlet (shamletFile)
import Text.Blaze.Html.Renderer.String (renderHtml)
import Text.Printf (printf)
import Text.Shakespeare.Text (textFile)

import Email
import Stripe
import Handler.Order (successKey, errorKey)

data Order = Order { orderCrns :: [Int]
                   , orderEmail :: Maybe (Maybe Text)
                   , orderPhoneNum :: Maybe (Maybe Text)
                   } deriving (Show, Read)

orderKey :: Text
orderKey = "_order"

setOrder :: Order -> Handler ()
setOrder = setSession orderKey . T.pack . show

getOrder :: Handler (Maybe Order)
getOrder = do
    mOrder <- getSessionWith orderKey
    case fmap (reads . T.unpack) mOrder of 
        Just [(x, "")] -> return $ Just x
        _ -> return Nothing

redirectSomewhere :: Handler RepHtml
redirectSomewhere = do
    mOrder <- getOrder
    case mOrder of
        Nothing -> redirect AccountR
        Just _ -> redirect ChooseCrnsR

getStartOrderR :: Handler RepHtml
getStartOrderR = redirectSomewhere

postStartOrderR :: Handler RepHtml
postStartOrderR = do
    deleteSession orderKey
    postAddCrnsR

getAddCrnsR :: Handler RepHtml
getAddCrnsR = redirectSomewhere

postAddCrnsR :: Handler RepHtml
postAddCrnsR = do
    (errors, crns) <- fmap parseCrns $ runInputPost $ ireq textField "crns"
    realCrns <- fmap (map $ sectionCrn . entityVal) $
        runDB $ selectList [SectionCrn <-. crns] []
    case realCrns of
        [] -> do
            _ <- addCrnsFromPostData False
            setSession errorKey "You must enter at least one valid CRN."
            redirectSomewhere
        _ -> do
            -- Apply the changes to the previously existing CRNs.
            -- Force it to succeed even if they removed everything,
            -- because they are adding at least one new CRN here.
            _ <- addCrnsFromPostData True
            -- Add the CRNs and indicate it.
            setSession successKey $ T.concat 
                ["Added ", fmtCrnList realCrns, "."]
            mOrder <- getOrder
            let oldCrns = case mOrder of
                    Nothing -> []
                    Just (Order x _ _) -> x
            setOrder $ case mOrder of
                Nothing -> Order (crns ++ oldCrns) Nothing Nothing
                Just order -> order { orderCrns = crns ++ oldCrns }
            -- Display a nice error message for bad CRNs.
            let diff = crns L.\\ realCrns
                error1 = if null diff then [] else
                    ["There are no courses with ", fmtCrnList diff, "."]
                error2 = if null errors then [] else
                    [if null error1 then ""  else " "
                    , "Ignoring invalid ", fmtCrnList errors, "."]
                message = error1 ++ error2
            when (not $ null message) $
                setSession errorKey $ T.concat message
            redirect ChooseCrnsR

getChooseCrnsR :: Handler RepHtml
getChooseCrnsR = do
    mOrder <- getOrder
    case mOrder of
        Nothing -> redirect AccountR
        Just (Order crns _ _) -> do
            givenSections <- runDB $ selectList [SectionCrn <-. crns] []
            let courseIds = L.sort $ L.nub $ map (normalizeCourseId .
                    sectionCourseId . entityVal) givenSections
                whereClause = T.intercalate " OR " $ map similarTo courseIds
                sql = T.concat
                    [ "SELECT ?? FROM section WHERE ", whereClause
                    , " ORDER BY section.course_id ASC, section.crn ASC" ]
            sqlResult <- fmap (map entityVal) $ runDB $ rawSql sql []
            let groups = zip courseIds $ L.groupBy sameCourseId sqlResult
            mErrorMessage <- consumeSession errorKey
            mSuccessMessage <- consumeSession successKey
            defaultLayout $
                $(widgetFile "choose-crns")
  where
    similarTo c = T.concat ["section.course_id SIMILAR TO '", p, "'"]
      where
        subj:num:_ = T.words c
        strippedNum = T.dropAround (fmap not isDigit) num
        p = T.concat [subj, " \\D{0,1}", strippedNum, "\\D{0,1}"]

postChooseCrnsR :: Handler RepHtml
postChooseCrnsR = do
    success <- addCrnsFromPostData False
    if success
        then redirect ContactInfoR
        else do
            setSession errorKey "You must choose at least one CRN."
            redirect ChooseCrnsR

addCrnsFromPostData :: Bool -> Handler Bool
addCrnsFromPostData force = do
    (postData, _) <- runRequestBody
    mOrder <- getOrder
    case mOrder of
        Nothing -> return False
        Just order -> do
            let crns = map (read . T.unpack . snd) postData
                success = force || (not $ null crns)
            when success $ setOrder $ order { orderCrns = crns }
            return success

getContactInfoR :: Handler RepHtml
getContactInfoR = do
    mOrder <- getOrder
    case mOrder of
        Nothing -> redirect AccountR
        Just (Order crns mmEmail mmPhoneNum) -> do
            user <- fmap (entityVal . fromJust) currentUser
            mErrorMessage <- consumeSession errorKey
            defaultLayout $ do
                setTitle "Contact information"
                $(widgetFile "contact-info")

postContactInfoR :: Handler RepHtml
postContactInfoR = do
    (mEmail, mRawPhoneNum) <- runInputPost $ (,)
        <$> iopt emailField "email"
        <*> iopt textField "phoneNum"
    mOrder <- getOrder
    case mOrder of
        Nothing -> redirect AccountR
        Just order -> do
            setOrder $ order { orderEmail = Just mEmail
                             , orderPhoneNum = Just mRawPhoneNum }
            if (isNothing mEmail) && (isNothing mRawPhoneNum)
                then do
                    setSession errorKey "You must fill in information for \
                        \at least one form of notifications."
                    redirect ContactInfoR
                else
                    case fmap validatePhoneNum mRawPhoneNum of
                        Just Nothing -> do
                            setSession errorKey
                                "Please enter a valid US phone number."
                            redirect ContactInfoR
                        x -> do
                            let mPhoneNum = fmap fromJust x
                            setOrder $ order
                                { orderEmail = Just mEmail
                                , orderPhoneNum = Just mPhoneNum}
                            redirect ReviewOrderR

getReviewOrderR :: Handler RepHtml
getReviewOrderR = do
    mOrder <- getOrder
    case mOrder of
        Nothing -> redirect AccountR
        Just (Order [] _ _) -> redirect ChooseCrnsR
        Just (Order crns (Just mEmail) (Just mPhoneNum)) -> do
            sections <- fmap (map entityVal) $
                runDB $ selectList [SectionCrn <-. crns]
                                   [Asc SectionCourseId, Asc SectionCrn]
            let courseIds = nubCourseIds sections
                price = offsetFees $ calcPrice sections courseIds
            extra <- getExtra
            mErrorMessage <- consumeSession errorKey
            defaultLayout $ do
                setTitle "Review order"
                $(widgetFile "review-order")
        _ -> redirect ContactInfoR

postReviewOrderR :: Handler RepHtml
postReviewOrderR = do
    (price, stripeToken) <- runInputPost $ (,)
        <$> ireq intField "price"
        <*> ireq textField "stripeToken"
    mOrder <- getOrder
    case mOrder of
        Nothing -> redirect AccountR
        Just (Order [] _ _) -> do
            setSession errorKey "You must select at least one CRN. \
                \Your card has not been charged."
            redirect ChooseCrnsR
        Just order@(Order crns (Just mEmail) (Just mPhoneNum)) -> do
            Entity userId user <- fmap fromJust currentUser
            manager <- fmap httpManager getYesod
            extra <- getExtra
            eCharge <- liftIO $ makeCharge manager extra stripeToken
                (T.pack $ show (price :: Int)) $ userEmail user
            case eCharge of
                Left charge -> do
                    deleteSession orderKey
                    sections <- runDB $ selectList [SectionCrn <-. crns] []
                    let req = SectionRequest userId
                            mEmail mPhoneNum False True True
                    runDB $ mapM_ (insert . req) $ map entityKey sections
                    let email = case mEmail of
                            Nothing -> userEmail user
                            Just x -> x
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
        _ -> do
            setSession errorKey "You must choose at least one form of \
                \notifications.i Your card has not been charged."
            redirect ContactInfoR

sendConfirmation :: Text -> Order -> Charge -> Handler ()
sendConfirmation email (Order crns mEmail mPhoneNum) charge = do
    sections <- fmap (map entityVal) $
        runDB $ selectList [SectionCrn <-. crns]
                           [Asc SectionCourseId, Asc SectionCrn]
    let courseIds = nubCourseIds sections
        text = toLazyText $
            $(textFile "templates/transaction-confirmation.text") ()
        html = LT.pack $ renderHtml
            $(shamletFile "templates/transaction-confirmation.hamlet")
    liftIO $ simpleMail to from subject text html [] >>= mySendmail
  where
    to = Address Nothing email
    from = noreplyAddr
    subject = "Bannerstalker transaction confirmation"

parseCrns :: Text -> ([Text], [Int])
parseCrns = partitionEithers . L.nub . map atoi . tokens
  where
    tokens = filter (not . T.null) . T.split (`elem` ", ")

atoi :: Text -> Either Text Int
atoi text = case reads $ T.unpack stripped of
    [(i, "")] -> Right i
    _ -> Left stripped
  where
    stripped = T.strip text

fmtCrnList :: Show a => [a] -> Text
fmtCrnList  = fmt . map (T.pack . show)
  where
    fmt [] = ""
    fmt [a] = T.concat ["CRN ", a]
    fmt [a, b] = T.concat ["CRNs ", a, " and ", b]
    fmt x = T.concat
        ["CRNs ", T.intercalate ", " $ L.init x, ", and ", L.last x]

-- For example: "PHYS 101L" -> "PHYS 101"
normalizeCourseId :: Text -> Text
normalizeCourseId courseId = T.unwords [subj, strippedNum]
  where
    subj:num:_ = T.words courseId
    strippedNum = T.dropAround (fmap not isDigit) num

nubCourseIds :: [Section] -> [Text]
nubCourseIds sections = L.sort $ L.nub courseIds
  where
    courseIds = map (normalizeCourseId . sectionCourseId) sections

groupSections :: [Section] -> [Text] -> [(Text, [Section])]
groupSections sections courseIds =
    zip courseIds $ L.groupBy sameCourseId sections

sameCourseId :: Section -> Section -> Bool
sameCourseId a b = c == d
  where
    [c, d] = map (normalizeCourseId . sectionCourseId) [a, b]

calcPrice :: [Section] -> [Text] -> Int
calcPrice sections courseIds =
    500 * numCourseIds + 100 * (length sections - numCourseIds)
  where
    numCourseIds = length courseIds

offsetFees :: Int -> Int
offsetFees p = round $ (fromIntegral (p + 30) / 0.971 :: Double)

makePriceChart :: [Section] -> [Text] -> Widget
makePriceChart sections courseIds = $(widgetFile "price-chart")
  where
    groups = groupSections sections courseIds
    initialPrice = calcPrice sections courseIds
    price = offsetFees initialPrice

formatPrice :: Int -> Text
formatPrice price =
    T.pack $ printf "%.2f" $ (/100.0) $ (fromIntegral price :: Float)
