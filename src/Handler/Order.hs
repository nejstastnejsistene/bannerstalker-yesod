module Handler.Order where

import Prelude
import Import
import Data.List (nub)
import Data.Maybe (catMaybes)
import Data.Text (pack)
import Text.Printf (printf)

getReviewOrderR :: Handler RepHtml
getReviewOrderR = do
    let crns = [10109, 10110, 10111]
        email = "pajohnson@email.wm.edu"::Text
        phoneNum = "+17034754114"::Text
        phoneCall = True

    mSections <- runDB $ mapM (getBy . UniqueCrn) crns
    let sections = map entityVal $ catMaybes mSections
        sectionsPrice = 500 + 100 * (length sections - 1)
        additionalPrice = if phoneCall then 300 else 0
        price = sectionsPrice + additionalPrice
    case nub $ map sectionCourseId sections of
        [courseId] -> defaultLayout $ do
            setTitle "Review Order"
            $(widgetFile "review-order")
        _ -> defaultLayout [whamlet|<h1>error|]
    where
        courseIdsEq = (\a b -> sectionCourseId a == sectionCourseId b)

formatPrice :: Int -> Text
formatPrice price =
    pack $ printf "%.2f" $ (/100.0) $ (fromIntegral price :: Float)
