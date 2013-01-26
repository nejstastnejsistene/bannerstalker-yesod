import Data.Char
import Data.Maybe
import Network.HTTP
import Network.URI
import qualified Text.HTML.TagSoup as TS

data SectionStatus = Open | Closed deriving (Show)
data Section = Unavailable |
               SectionData { semester :: String
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
        [strCrn, subject, courseId, title,
            instructor, days, times, _, _, _, _, strStatus] =
    let crn = read strCrn
        status = case strStatus of
                    "OPEN"   -> Just Open
                    "CLOSED" -> Just Closed
                    _        -> Nothing
        section = SectionData semester crn subject
                    courseId title instructor days times status
    in if isNothing status
        then Left $ "Unkown status: " ++ strStatus
        else Right section
makeSection semester args = Left "Wrong number of arguments"

main = let semester = "201320" in do
    response <- requestCourseList "201320" "ARAB"
    case response of
        Left err  -> putStrLn $ "Error:\n" ++ err
        Right html ->
            let rows = parseCourseList html
                sections = map (makeSection semester) rows
            in putStrLn $ show sections
