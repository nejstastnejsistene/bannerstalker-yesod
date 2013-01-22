import Data.Char
import Data.Maybe
import Text.HTML.TagSoup
import Network.HTTP
import Network.URI

uri = fromJust $ parseURI url where
url = "http://courselist.wm.edu/wmcourseschedule/courseinfo/searchresults"

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
     
parseCourseList :: String -> IO ()
parseCourseList html = do
    putStrLn $ show $ filteredRows !! 5
    where
        table = head $ getTagContents "table" $ parseTags html
        rows = getTagContents "tr" table
        filteredRows = map (getTagContents "td") rows
        -- Extension of sections that trims tags coming after
        -- the first closing tag.
        getTagContents :: String -> [Tag String] -> [[Tag String]]
        getTagContents tagName tags =
            let tagContents = sections (~== ("<"++tagName++">")) tags
                trimTags = takeWhile (not . isTagCloseName tagName)
                in map trimTags (map tail tagContents)

main = do
    asdf <- requestCourseList "201320" "ARAB"
    case asdf of
        Left x  -> putStrLn $ "Error:\n" ++ x
        Right x -> parseCourseList x
