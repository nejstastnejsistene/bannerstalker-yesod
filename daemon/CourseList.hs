module CourseList (fetchCourseList) where

import Prelude
import qualified Data.ByteString as B
import Data.Char
import Data.Conduit
import Data.Conduit.List (consume)
import Data.Either
import Data.Maybe
import qualified Data.Text as T
import Data.Text.Encoding
import Text.XML.HXT.Parser.XhtmlEntities
import Text.Regex.PCRE
import Network.HTTP.Conduit
import Network.HTTP.Types

import Email
import Model

url :: String
url = "https://courselist.wm.edu/wmcourseschedule/courseinfo/searchresults"

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

-- Went a little crazy and wrote a function to decode html entity refs...
-- Based off code from the now defunct web-encodings package.
decodeHtml :: T.Text -> T.Text
decodeHtml text = case T.uncons text of
    Nothing -> ""
    Just ('&', xs) ->
        let (before, after) = T.breakOn ";" xs
            remaining = T.tail after
        in case parseEntityRef $ T.unpack before of
            Nothing -> decodeHtml remaining
            Just ch -> T.cons ch $ decodeHtml remaining
    Just (x, xs) -> T.cons x $ decodeHtml xs
    where
        parseEntityRef ('#':'x':hex) = readHexChar hex
        parseEntityRef ('#':'X':hex) = readHexChar hex
        parseEntityRef ('#':    dec) = readDecChar dec
        parseEntityRef s = case lookup s xhtmlEntities of
            Nothing -> Nothing
            Just dec -> Just $ chr dec
        readHexChar s = readDecChar $ "0x" ++ s
        readDecChar s = case reads s of
            (i, _):_ -> Just $ chr (i :: Int)
     
-- Creates a Section given the semester and a list of arguments.
makeSection :: SemesterId -> [B.ByteString] -> Either T.Text Section
makeSection semester args = case args' of
    [crn, courseId, _, title, instr,  _, days, times, _, _, _, status] ->
        let crn' = read $ T.unpack crn
            courseId'= T.unwords $ take 2 $ T.words courseId
            status' = case status of
                        "OPEN"   -> Just Open
                        "CLOSED" -> Just Closed
                        _        -> Nothing
        in if isNothing status'
            then Left $ T.concat ["Unkown status: ", status]
            else Right $ Section semester crn' courseId'
                            title instr days times $ fromJust status'
    _ -> Left "Wrong number of arguments to makeSection"
    where
        args' = map (T.strip . decodeHtml . decodeUtf8) args

fetchCourseList :: Manager
                   -> SemesterId
                   -> T.Text
                   -> IO (Either T.Text [Section])
fetchCourseList manager semesterId semesterCode = do
    response <- requestCourseList manager semesterCode
    return $ case response of
        Left err -> Left err
        Right html ->
            let rows = map tail $ html =~ pattern
                eitherSections = map (makeSection semesterId) rows
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
