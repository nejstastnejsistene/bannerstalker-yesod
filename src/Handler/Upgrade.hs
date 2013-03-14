{-# LANGUAGE DoAndIfThenElse #-}
module Handler.Upgrade where 

import Prelude (head)
import Import
import Data.Maybe
import Data.Text (pack, unpack)
import Text.Printf

import Stripe

upgradeInfoKey, upgradeErrorKey :: Text
upgradeInfoKey = "_UpgradeR_mInfoMessage"
upgradeErrorKey = "_UpgradeR_mErrorMessage"

getUpgradeRootR :: Handler RepHtml
getUpgradeRootR = do
    code <- fmap (semesterCode . entityVal . head) $
        runDB $ selectList [SemesterActive ==. True]
                           [Desc SemesterCode, LimitTo 1]
    redirectUltDest $ UpgradeR code

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
    mInfoMessage <- getSessionWith upgradeInfoKey
    mErrorMessage <- getSessionWith upgradeErrorKey
    deleteSession upgradeInfoKey
    deleteSession upgradeErrorKey
    defaultLayout $ do
        setTitle "Upgrade"
        $(widgetFile "upgrade")

postUpgradeR :: Text -> Handler RepHtml
postUpgradeR code = do
    semesterId <- fmap (entityKey . fromJust) $
        runDB $ getBy $ UniqueSemester code
    Entity userId user <- fmap fromJust currentUser
    Entity privId (Privilege _ _ currentLevel) <- fmap fromJust $
        runDB $ getBy $ UniquePrivilege userId semesterId
    (level, stripeToken) <- runInputPost $ (,)
        <$> ireq textField "level"
        <*> ireq textField "stripeToken"
    let targetLevel = read $ unpack level :: PrivilegeLevel
        price = getPrice currentLevel targetLevel
    if price <= 0 then
        error "upgrading must go up a level"
    else do
        manager <- fmap httpManager getYesod
        extra <- getExtra
        eCharge <- liftIO $ makeCharge manager extra
            stripeToken (pack $ show price) $ userEmail user
        case eCharge of
            Left charge -> do
                runDB $ update privId [PrivilegeLevel =. targetLevel]
                setSession upgradeInfoKey "Transaction successful! (Note to self: send a confirmation email)"
            Right err -> do
                setSession upgradeErrorKey $ errorMessage err
        redirectUltDest $ UpgradeR code

getPrice :: PrivilegeLevel -> PrivilegeLevel -> Int
getPrice current target = price target - price current
    where
        price Level1 = 0
        price Level2 = 500
        price Level3 = 1000
        price Admin = 100000

getPriceF :: PrivilegeLevel -> PrivilegeLevel -> Text
getPriceF current target = pack $ printf "%.2f" price
    where
        price = (/100.0) $ fromIntegral $ getPrice current target :: Float

levelName :: PrivilegeLevel -> Text
levelName Level1 = "Silver"
levelName Level2 = "Gold"
levelName Level3 = "Platinum"
levelName Admin = "Admin"
