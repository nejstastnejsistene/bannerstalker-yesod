import Data.Char
import Data.Maybe
import Network.HTTP
import Network.URI

uri = fromJust $ parseURI url where
url = "http://courselist.wm.edu/wmcourseschedule/courseinfo/searchresults"

requestCourseList :: String -> String -> IO ()
requestCourseList semester subject = do
    response <- simpleHTTP request
    putStrLn $ show response
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
     

main = do
   requestCourseList "201320" "ARAB"
