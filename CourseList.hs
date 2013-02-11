module CourseList (fetchCourseList) where

import Prelude
import Data.Either
import Data.Maybe
import Network.HTTP
import Network.URI
import qualified Text.HTML.TagSoup as TS

import Model

url :: String
url = "http://courselist.wm.edu/wmcourseschedule/courseinfo/searchresults"

-- Requests the raw html from the courselist website.
requestCourseList :: String -> String -> IO (Either String String)
requestCourseList semester subject = do
    response <- simpleHTTP request
    case response of
        Left x  -> return $ Left ("Error connecting: " ++ show x)
        Right r -> do
            case rspCode r of
                (2,0,0) -> return $ Right (rspBody r)
                _       -> return $ Left (show r)
    where
        uri = fromJust $ parseURI url
        postQuery = [("term_code", semester),
                 ("term_subj", subject),
                 ("attr",      "0"),
                 ("levl",      "0"),
                 ("status",    "0"),
                 ("search",    "Search"),
                 ("sort",      "crn_key"),
                 ("order",     "asc")]
        postData = urlEncodeVars postQuery
        headers = [
            Header HdrContentType "application/x-www-form-urlencoded",
            Header HdrContentLength . show $ length postData ]
        request = Request {
            rqURI = uri,
            rqMethod = POST,
            rqHeaders = headers,
            rqBody = postData }
     

-- Parses the raw html into a table of strings from the td tags.
parseCourseList :: String -> [[String]]
parseCourseList html = do
    extractTagString rows
    where
        table = head $ getTagContents "table" $ TS.parseTags html
        rows = map (getTagContents "td") $ getTagContents "tr" table
        -- Extension of sections that trims tags coming after
        -- the first closing tag.
        getTagContents :: String -> [TS.Tag String] -> [[TS.Tag String]]
        getTagContents tagName tags =
            let sections = TS.sections (TS.~== ("<"++tagName++">")) tags
                -- Take only the tags between the start and end tags.
                trimTags = takeWhile (not . TS.isTagCloseName tagName)
                in map (trimTags . tail) sections 
        -- Converts the list of TagStrings to 2d array of strings
        extractTagString :: [[[TS.Tag String]]] -> [[String]]
        extractTagString rowsList = 
            let extract = map $ map $ head . (map TS.fromTagText)
            in filter (\x -> x /= []) $ (extract rowsList)


-- Creates a Section given the semester and a list of arguments.
makeSection :: String -> [String] -> Either String Section
makeSection semester
        [rawCrn, rawCourseId, _, title, instructor,
                days, times, _, _, _, _, rawStatus] =
    let crn = read rawCrn
        courseIdWords = words rawCourseId
        subject = head courseIdWords
        courseId = courseIdWords !! 1
        status = case rawStatus of
                    "OPEN"   -> Just Open
                    "CLOSED" -> Just Closed
                    _        -> Nothing
    in if isNothing status
        then Left $ "Unkown status: " ++ rawStatus
        else Right $ Section semester crn subject courseId
                        title instructor days times $ fromJust status
makeSection _ _ = Left "Wrong number of arguments"


fetchCourseList :: String -> String -> IO (Either String [Section])
fetchCourseList semester subject = do
    response <- requestCourseList semester subject
    case response of
        Left err -> return $ Left err
        Right html ->
            let rows = parseCourseList html
                eitherSections = map (makeSection semester) rows
                (errors, sections) = partitionEithers eitherSections
            in case errors of
                [] -> return $ Right sections
                _  -> return $ Left $ head errors
