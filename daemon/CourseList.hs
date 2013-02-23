module CourseList (fetchCourseList) where

import Prelude
import Control.Exception
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Conduit
import Data.Either
import Data.Maybe
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Encoding (decodeUtf8)
import Text.Regex.PCRE
import Network.HTTP.Conduit
import Network.HTTP.Types

import Model

url :: String
url = "http://courselist.wm.edu/wmcourseschedule/courseinfo/searchresults"

-- Requests the raw html from the courselist website.
requestCourseList :: Manager -> T.Text -> IO (Either T.Text BL.ByteString)
requestCourseList manager semester = do
    runResourceT $ do
        defReq <- parseUrl url
        let request = urlEncodedBody params $ defReq
                        { checkStatus = \_ _ -> Nothing
                        , responseTimeout = Just 60000000 -- 1 minute
                        }
        response <- httpLbs request manager
        let status = responseStatus response
        case status of
            Status 200 "OK" -> return $ Right $ responseBody response
            _ -> return $ Left $ T.pack $ show status
    where
        params :: [(BS.ByteString, BS.ByteString)]
        params = [("term_code", encodeUtf8 semester)
                 ,("term_subj", "0")
                 ,("attr",      "0")
                 ,("levl",      "0")
                 ,("status",    "0")
                 ,("search",    "Search")
                 ,("sort",      "crn_key")
                 ,("order",     "asc")]
     
-- Creates a Section given the semester and a list of arguments.
makeSection :: T.Text -> [BL.ByteString] -> Either T.Text Section
makeSection semester args = case args' of
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
    _ -> Left $ T.concat [ "Wrong number of arguments to makeSection: "
                         , T.pack $ show (args' :: [T.Text])
                         ]
    where args' = map (T.strip . toStrict . decodeUtf8) args

fetchCourseList :: Manager -> T.Text -> IO (Either T.Text [Section])
fetchCourseList manager semester = do
    response <- requestCourseList manager semester
    case response of
        Left err -> return $ Left err
        Right html ->
            let rows = map tail $ html =~ pattern
                eitherSections = map (makeSection semester) rows
                (errors, sections) = partitionEithers eitherSections
            in case errors of
                [] -> return $ Right sections
                _  -> do
                    putStrLn $ show $ head errors
                    return $ Left $ T.concat
                        [ T.pack $ show $ length errors
                        , " errors: e.g. "
                        , T.pack $ show $ head errors
                        ]
    where
        pattern :: BL.ByteString
        pattern = BL.concat $ replicate 12 "<td[^>]*>([^<]+)</td>\\s*"
