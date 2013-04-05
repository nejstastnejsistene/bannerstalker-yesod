module Handler.NewOrder where

import Import
import Data.Either (partitionEithers)
import qualified Data.List as L
import qualified Data.Text as T

import Handler.Order (successKey, errorKey)

data NewOrder = NewOrder { newOrderCrns :: [Int]
                         } deriving (Show, Read)

newOrderKey :: Text
newOrderKey = "_newOrder"

setOrder :: NewOrder -> Handler ()
setOrder = setSession newOrderKey . T.pack . show

getOrder :: Handler (Maybe NewOrder)
getOrder = do
    mOrder <- getSessionWith newOrderKey
    return $ fmap (read . T.unpack) mOrder

getNewStartOrderR :: Handler RepHtml
getNewStartOrderR = do
    mOrder <- getOrder
    case mOrder of
        Nothing -> redirect AccountR
        Just _ -> redirect NewChooseCrnsR

postNewStartOrderR :: Handler RepHtml
postNewStartOrderR = do
    deleteSession newOrderKey
    postNewOrderAddCrnsR

postNewOrderAddCrnsR :: Handler RepHtml
postNewOrderAddCrnsR = do
    (errors, crns) <- fmap parseCrns $ runInputPost $ ireq textField "crns"
    case crns of
        [] -> do
            setSession errorKey "You must enter at least one valid CRN."
            redirect AccountR
        _ -> do
            let crnPhrase = ["Added ", fmtCrnList crns, "."]
                errorPhrase = if null errors then []
                    else [" Ignoring invalid ", fmtCrnList errors, "."]
            setSession successKey $ T.concat $ crnPhrase ++ errorPhrase 
            setSession newOrderKey $ T.pack $ show $ NewOrder crns
            redirect NewChooseCrnsR
  where
    parseCrns :: Text -> ([Text], [Int])
    parseCrns = partitionEithers . L.nub . map atoi . T.split (`elem` ", ")

    atoi :: Text -> Either Text Int
    atoi text = case reads $ T.unpack stripped of
        [(i, "")] -> Right i
        _ -> Left stripped
      where
        stripped = T.strip text

getNewChooseCrnsR :: Handler RepHtml
getNewChooseCrnsR = do
    mOrder <- getSessionWith newOrderKey
    case fmap (read . T.unpack) mOrder of
        Nothing -> redirect AccountR
        Just (NewOrder crns) -> do
            mErrorMessage <- consumeSession errorKey
            mSuccessMessage <- consumeSession successKey
            defaultLayout [whamlet|
$newline never
^{showMessage SuccessMessage mSuccessMessage Nothing}
^{showMessage ErrorMessage mErrorMessage Nothing}
<p>#{fmtCrnList crns}
|] 

postNewChooseCrnsR :: Handler RepHtml
postNewChooseCrnsR = do
    redirect NewContactInfoR

getNewContactInfoR :: Handler RepHtml
getNewContactInfoR = do
    defaultLayout [whamlet|not implemented|]

postNewContactInfoR :: Handler RepHtml
postNewContactInfoR = do
    redirect ReviewOrderR

getNewReviewOrderR :: Handler RepHtml
getNewReviewOrderR = do
    defaultLayout [whamlet|not implemented|]

postNewReviewOrderR :: Handler RepHtml
postNewReviewOrderR = do
    defaultLayout [whamlet|not implemented|]

fmtCrnList :: Show a => [a] -> Text
fmtCrnList  = fmt . map (T.pack . show)
  where
    fmt [] = ""
    fmt [a] = T.concat ["CRN ", a]
    fmt [a, b] = T.concat ["CRNs ", a, " and ", b]
    fmt x = T.concat
        ["CRNs ", T.intercalate ", " $ L.init x, ", and ", L.last x]
