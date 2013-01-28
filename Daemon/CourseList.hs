{-
module CourseList
    (   SectionStatus (Open, Closed, Unavailable)
    ,   Section
    ,   getCourseList
    ) where
-}


import Data.Char
import Data.Maybe
import Data.Either
import Network.HTTP
import Network.URI
import qualified Text.HTML.TagSoup as TS
 

data SectionStatus = Open | Closed | Unavailable deriving (Show)
data Section = Section { semester :: String
                       , crn :: Int
                       , subject :: String
                       , courseId :: String
                       , title :: String
                       , instructor :: String
                       , days :: String
                       , times :: String
                       , status :: Maybe SectionStatus
                       } deriving (Show)


uri = fromJust $ parseURI url where
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
        query = [("term_code", semester),
                 ("term_subj", subject),
                 ("attr",      "0"),
                 ("levl",      "0"),
                 ("status",    "0"),
                 ("search",    "Search"),
                 ("sort",      "crn_key"),
                 ("order",     "asc")]
        postData = urlEncodeVars query
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
    extractTagText rows
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
        -- Converts the list of TagTexts to 2d array of strings
        extractTagText :: [[[TS.Tag String]]] -> [[String]]
        extractTagText rows = 
            let extract = map $ map $ head . (map TS.fromTagText)
            in filter (\x -> x /= []) $ (extract rows)


-- Creates a Section given the semester and a list of arguments.
makeSection :: String -> [String] -> Either String Section
makeSection semester
        [strCrn, rawCourseId, _, title, instructor,
                days, times, _, _, _, _, strStatus] =
    let crn = read strCrn
        courseIdWords = words rawCourseId
        subject = head courseIdWords
        courseId = courseIdWords !! 1
        status = case strStatus of
                    "OPEN"   -> Just Open
                    "CLOSED" -> Just Closed
                    _        -> Nothing
        section = Section semester crn subject courseId
                        title instructor days times status
    in if isNothing status
        then Left $ "Unkown status: " ++ strStatus
        else Right section
makeSection semester args = Left "Wrong number of arguments"


checkSectionErrors :: [Either String Section] -> Either String [Section]
checkSectionErrors eitherSections =
    let errors =  lefts eitherSections
    in case errors of
        [] -> Right $ rights eitherSections
        _  -> Left $ head errors


getCourseList :: String -> String -> IO (Either String [Section])
getCourseList semester subject = do
    response <- requestCourseList semester subject
    case response of
        Left error -> return $ Left error
        Right html ->
            let rows = parseCourseList html
                sections = map (makeSection semester) rows
            in return $ checkSectionErrors sections


main = let semester = "201320" in do
    courseList <- getCourseList "201320" "ARAB"
    case courseList of
        Left error -> putStrLn $ "Error:\n" ++ error
        Right courseList -> putStrLn $ show courseList
