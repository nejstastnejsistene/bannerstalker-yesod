{-# LANGUAGE DoAndIfThenElse #-}
module Handler.Upgrade where 

import Prelude (head)
import Import
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import Network.Mail.Mime
import Text.Blaze.Html.Renderer.String
import Text.Hamlet
import Text.Printf

import Email
import Stripe

    {-
upgradeSuccessKey, upgradeErrorKey :: Text
upgradeSuccessKey = "_UpgradeR_upgradeSuccess"
upgradeErrorKey = "_UpgradeR_upgradeError"

getUpgradeRootR :: Handler RepHtml
getUpgradeRootR = do
    code <- fmap (semesterCode . entityVal . head) $
        runDB $ selectList [SemesterActive ==. True]
                           [Desc SemesterCode, LimitTo 1]
    redirect $ UpgradeR code

getUpgradeR :: Text -> Handler RepHtml
getUpgradeR code = do
    Entity semesterId (Semester _ name _) <- fmap fromJust $
        runDB $ getBy $ UniqueSemester code
    Entity userId user <- fmap fromJust currentUser
    currentLevel <- fmap (privilegeLevel . entityVal . fromJust) $
        runDB $ getBy $ UniquePrivilege userId semesterId
    otherSemesters <- fmap (map entityVal) $ runDB $ selectList
        [SemesterCode !=. code, SemesterActive ==. True]
        [Asc SemesterCode]
    mSuccessMessage <- consumeSession upgradeSuccessKey
    mErrorMessage <- consumeSession upgradeErrorKey
    defaultLayout $ do
        setTitle "Upgrade"
        -- $(widgetFile "upgrade")
        [whamlet|<h1>Fucking piviting!|]

postUpgradeR :: Text -> Handler RepHtml
postUpgradeR code = do
    {-
    Entity semesterId (Semester _ name _) <- fmap fromJust $
        runDB $ getBy $ UniqueSemester code
    Entity userId user <- fmap fromJust currentUser
    Entity privId (Privilege _ _ currentLevel) <- fmap fromJust $
        runDB $ getBy $ UniquePrivilege userId semesterId
    (level, stripeToken) <- runInputPost $ (,)
        <$> ireq textField "level"
        <*> ireq textField "stripeToken"
    let targetLevel = read $ T.unpack level :: PrivilegeLevel
        price = getPrice currentLevel targetLevel
    if price <= 0 then
        error "upgrading must go up a level"
    else do
        manager <- fmap httpManager getYesod
        extra <- getExtra
        eCharge <- liftIO $ makeCharge manager extra
            stripeToken (T.pack $ show price) $ userEmail user
        case eCharge of
            Left charge -> do
                runDB $ update privId [PrivilegeLevel =. targetLevel]
                sendConfirmation userId name targetLevel charge
                setSession upgradeSuccessKey $ T.concat
                    [ "Transaction successful!"
                    , " We sent you a confirmation email." ]
            Right err -> do
                setSession upgradeErrorKey $ T.concat
                    [errorMessage err, " Your card has not been charged."]
    -}
        redirect $ UpgradeR code

sendConfirmation :: UserId -> Text -> PrivilegeLevel -> Charge -> Handler ()
sendConfirmation userId semester level charge = do
    User email _ _ _ _ <- fmap fromJust $ runDB $ get userId
    let to = Address Nothing email
        from = noreplyAddr
        subject = "Bannerstalker transaction confirmation"
        text = LT.pack $ renderHtml
                $(shamletFile "templates/transaction-confirmation-text.hamlet")
        html = LT.pack $ renderHtml
                $(shamletFile "templates/transaction-confirmation-html.hamlet")
    liftIO $ simpleMail to from subject text html [] >>= mySendmail

getPrice :: PrivilegeLevel -> PrivilegeLevel -> Int
getPrice current target = price target - price current
    where
        price Level1 = 0
        price Level2 = 500
        price Level3 = 1000
        price Admin = 100000

formatPrice :: Int -> Text
formatPrice price =
    T.pack $ printf "%.2f" $ (/100.0) $ (fromIntegral price :: Float)

levelName :: PrivilegeLevel -> Text
levelName Level1 = "Silver"
levelName Level2 = "Gold"
levelName Level3 = "Platinum"
levelName Admin = "Admin"
    -}
