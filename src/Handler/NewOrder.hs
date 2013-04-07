module Handler.NewOrder where

import Import
import Control.Monad (when)
import Data.Char (isDigit)
import Data.Either (partitionEithers)
import Data.Maybe (fromJust)
import qualified Data.List as L
import qualified Data.Text as T
import Database.Persist.GenericSql

import Handler.Order (successKey, errorKey)

data NewOrder = NewOrder { newOrderCrns :: [Int]
                         , newOrderEmail :: Maybe Text
                         , newOrderPhoneNum :: Maybe Text
                         } deriving (Show, Read)

newOrderKey :: Text
newOrderKey = "_newOrder"

setOrder :: NewOrder -> Handler ()
setOrder = setSession newOrderKey . T.pack . show

getOrder :: Handler (Maybe NewOrder)
getOrder = do
    mOrder <- getSessionWith newOrderKey
    return $ fmap (read . T.unpack) mOrder

redirectSomewhere :: Handler RepHtml
redirectSomewhere = do
    mOrder <- getOrder
    case mOrder of
        Nothing -> redirect AccountR
        Just _ -> redirect NewChooseCrnsR

getNewStartOrderR :: Handler RepHtml
getNewStartOrderR = redirectSomewhere

postNewStartOrderR :: Handler RepHtml
postNewStartOrderR = do
    deleteSession newOrderKey
    postNewOrderAddCrnsR

getNewOrderAddCrnsR :: Handler RepHtml
getNewOrderAddCrnsR = redirectSomewhere

postNewOrderAddCrnsR :: Handler RepHtml
postNewOrderAddCrnsR = do
    (errors, crns) <- fmap parseCrns $ runInputPost $ ireq textField "crns"
    realCrns <- fmap (map $ sectionCrn . entityVal) $
        runDB $ selectList [SectionCrn <-. crns] []
    case realCrns of
        [] -> do
            setSession errorKey "You must enter at least one valid CRN."
            redirectSomewhere
        _ -> do
            -- Add the CRNs and indicate it.
            setSession successKey $ T.concat 
                ["Added ", fmtCrnList realCrns, "."]
            mOrder <- getOrder
            let oldCrns = case mOrder of
                    Nothing -> []
                    Just (NewOrder x _ _) -> x
            setOrder $ NewOrder (crns ++ oldCrns) Nothing Nothing
            -- Display a nice error message for bad CRNs.
            let diff = crns L.\\ realCrns
                error1 = if null diff then [] else
                    ["There are no courses with ", fmtCrnList diff, "."]
                error2 = if null errors then [] else
                    [if null error1 then ""  else " "
                    , "Ignoring invalid ", fmtCrnList errors, "."]
                errorMessage = error1 ++ error2
            when (not $ null errorMessage) $
                setSession errorKey $ T.concat errorMessage
            redirect NewChooseCrnsR

getNewChooseCrnsR :: Handler RepHtml
getNewChooseCrnsR = do
    mOrder <- getOrder
    case mOrder of
        Nothing -> redirect AccountR
        Just (NewOrder crns _ _) -> do
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
                $(widgetFile "new-order")
  where
    similarTo c = T.concat ["section.course_id SIMILAR TO '", p, "'"]
      where
        subj:num:_ = T.words c
        strippedNum = T.dropAround (fmap not isDigit) num
        p = T.concat [subj, " \\D{0,1}", strippedNum, "\\D{0,1}"]
    sameCourseId a b = c == d
      where
        [c, d] = map (normalizeCourseId . sectionCourseId) [a, b]

postNewChooseCrnsR :: Handler RepHtml
postNewChooseCrnsR = do
    (postData, _) <- runRequestBody
    setOrder $ NewOrder
        (map (read . T.unpack . snd) postData) Nothing Nothing
    redirect NewContactInfoR

getNewContactInfoR :: Handler RepHtml
getNewContactInfoR = do
    mOrder <- getOrder
    case mOrder of
        Nothing -> redirect AccountR
        Just (NewOrder crns _ _) -> do
            user <- fmap (entityVal . fromJust) currentUser
            mErrorMessage <- consumeSession errorKey
            defaultLayout $ do
                setTitle "Contact information"
                $(widgetFile "new-contact-info")

postNewContactInfoR :: Handler RepHtml
postNewContactInfoR = do
    redirect NewReviewOrderR

getNewReviewOrderR :: Handler RepHtml
getNewReviewOrderR = do
    defaultLayout [whamlet|not implemented|]

postNewReviewOrderR :: Handler RepHtml
postNewReviewOrderR = do
    defaultLayout [whamlet|not implemented|]

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
