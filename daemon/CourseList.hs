module CourseList (fetchCourseList) where

import Prelude
import Control.Exception
import Data.ByteString (ByteString)
import Data.Conduit
import Data.Either
import Data.Maybe
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Encoding (decodeUtf8)
import Network.HTTP.Conduit
import Network.HTTP.Types
import qualified Text.HTML.TagSoup as TS

import Model

url :: String
url = "http://courselist.wm.edu/wmcourseschedule/courseinfo/searchresults"

-- Requests the raw html from the courselist website.
requestCourseList :: Manager -> T.Text -> IO (Either T.Text T.Text)
requestCourseList manager semester =
    runResourceT $ do
        defReq <- parseUrl url
        let request = urlEncodedBody params $ defReq
                        { checkStatus = \_ _ -> Nothing
                        , responseTimeout = Just 60000000 -- 1 minute
                        }
        response <- httpLbs request manager
        let status = responseStatus response
        case status of
            Status 200 "OK" ->
                let body = toStrict $ decodeUtf8 $ responseBody response
                in return $ Right body
            _ -> return $ Left $ T.pack $ show status
    where
        params :: [(ByteString, ByteString)]
        params = [("term_code", encodeUtf8 semester)
                 ,("term_subj", "0")
                 ,("attr",      "0")
                 ,("levl",      "0")
                 ,("status",    "0")
                 ,("search",    "Search")
                 ,("sort",      "crn_key")
                 ,("order",     "asc")]
     

-- Parses the raw html into a table of strings from the td tags.
parseCourseList :: T.Text -> [[T.Text]]
parseCourseList html = do
    extractTagString rows
    where
        table = head $ getTagContents "table" $ TS.parseTags html
        rows = map (getTagContents "td") $ getTagContents "tr" table
        -- Extension of sections that trims tags coming after
        -- the first closing tag.
        getTagContents :: T.Text -> [TS.Tag T.Text] -> [[TS.Tag T.Text]]
        getTagContents tagName tags =
            let brTag = T.unpack $ T.concat ["<", tagName, ">"]
                sections = TS.sections (TS.~== brTag) tags
                -- Take only the tags between the start and end tags.
                trimTags = takeWhile (not . TS.isTagCloseName tagName)
                in map (trimTags . tail) sections 
        -- Converts the list of TagStrings to 2d array of strings
        extractTagString :: [[[TS.Tag T.Text]]] -> [[T.Text]]
        extractTagString rowsList = 
            -- Also strip all of the td tags.
            let toString = T.strip . TS.fromTagText
                extract = map $ map $ head . (map toString)
            in filter (not . null) $ extract rowsList


-- Creates a Section given the semester and a list of arguments.
makeSection :: T.Text -> [T.Text] -> Either T.Text Section
makeSection semester
        [rawCrn, rawCourseId, _, title, instructor,
                _, days, times, _, _, _, rawStatus] =
    let crn = read $ T.unpack rawCrn
        courseIdWords = T.words rawCourseId
        subject = head courseIdWords
        courseId = courseIdWords !! 1
        status = case rawStatus of
                    "OPEN"   -> Just Open
                    "CLOSED" -> Just Closed
                    _        -> Nothing
    in if isNothing status
        then Left $ T.concat ["Unkown status: ", rawStatus]
        else Right $ Section semester crn subject courseId
                        title instructor days times $ fromJust status
makeSection _ _ = Left "Wrong number of arguments"


unsafeFetchCourseList :: Manager -> T.Text -> IO (Either T.Text [Section])
unsafeFetchCourseList manager semester = do
    response <- requestCourseList manager semester
    case response of
        Left err -> return $ Left err
        Right html ->
            let rows = parseCourseList html
                eitherSections = map (makeSection semester) rows
                (errors, sections) = partitionEithers eitherSections
            in case errors of
                [] -> return $ Right sections
                _  -> return $ Left $ T.pack $ show errors

fetchCourseList :: Manager -> T.Text -> IO (Either T.Text [Section])
fetchCourseList manager semester = do
    result <- try $ unsafeFetchCourseList manager semester
    case result of
        Left ex -> return $ Left $ T.pack $ show (ex :: SomeException)
        Right x -> return x
