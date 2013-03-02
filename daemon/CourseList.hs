module CourseList (fetchCourseList) where

import Prelude
import qualified Data.ByteString as B
import Data.Conduit
import Data.Conduit.List (consume)
import Data.Either
import Data.Maybe
import qualified Data.Text as T
import Data.Text.Encoding
import Text.Regex.PCRE
import Network.HTTP.Conduit
import Network.HTTP.Types

import Email
import Model

url :: String
url = "http://courselist.wm.edu/wmcourseschedule/courseinfo/searchresults"

-- Requests the raw html from the courselist website.
requestCourseList :: Manager -> T.Text -> IO (Either T.Text B.ByteString)
requestCourseList manager semester = do
    runResourceT $ do
        defReq <- parseUrl url
        let request = urlEncodedBody params $ defReq
                        { checkStatus = \_ _ -> Nothing
                        , responseTimeout = Just 60000000 -- 1 minute
                        }
        response <- http request manager
        body <- fmap B.concat $ responseBody response $$+- consume
        case responseStatus response of
            Status 200 "OK" -> return $ Right body
            Status code mesg -> return $ Left $ T.pack $
                "Status: " ++ show code ++ " " ++ show mesg
    where
        params :: [(B.ByteString, B.ByteString)]
        params = [("term_code", encodeUtf8 semester)
                 ,("term_subj", "0")
                 ,("attr",      "0")
                 ,("levl",      "0")
                 ,("status",    "0")
                 ,("search",    "Search")
                 ,("sort",      "crn_key")
                 ,("order",     "asc")]
     
-- Creates a Section given the semester and a list of arguments.
makeSection :: T.Text -> [B.ByteString] -> Either T.Text Section
makeSection semester args = case map (T.strip . decodeUtf8) args of
    [crn, courseId, _, title, instr,  _, days, times, _, _, _, status] ->
        let crn' = read $ T.unpack crn
            subject:courseId':_ = T.words courseId
            status' = case status of
                        "OPEN"   -> Just Open
                        "CLOSED" -> Just Closed
                        _        -> Nothing
        in if isNothing status'
            then Left $ T.concat ["Unkown status: ", status]
            else Right $ Section semester crn' subject courseId'
                            title instr days times $ fromJust status'
    _ -> Left "Wrong number of arguments to makeSection"

fetchCourseList :: Manager -> T.Text -> IO (Either T.Text [Section])
fetchCourseList manager semester = do
    response <- requestCourseList manager semester
    return $ case response of
        Left err -> Left err
        Right html ->
            let rows = map tail $ html =~ pattern
                eitherSections = map (makeSection semester) rows
                (errors, sections) = partitionEithers eitherSections
            in case errors of
                [] -> Right sections
                _  -> Left $ T.concat
                    [ T.pack $ show $ length errors
                    , " errors: e.g. "
                    , T.pack $ show $ head errors
                    ]
    where
        pattern :: B.ByteString
        pattern = B.concat $ replicate 12 "<td[^>]*>([^<]+)</td>\\s*"
