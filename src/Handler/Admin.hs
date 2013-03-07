module Handler.Admin where

import Import
import Data.Maybe
import qualified Data.Text as T

getAdminR :: Handler RepHtml
getAdminR = defaultLayout $ do
    setTitle "Admin"
    $(widgetFile "admin")

getAdminUsersR :: Handler RepHtml
getAdminUsersR = do
    users <- runDB $ selectList [] []
    defaultLayout $ do
        setTitle "Users"
        $(widgetFile "admin-users")

data SemesterPriv = SemesterPriv Text Text PrivilegeLevel

getAdminEditUserR :: UserId -> Handler RepHtml
getAdminEditUserR userId = getAdminEditUserHelper userId Nothing

getAdminEditUserHelper :: UserId -> Maybe Text -> Handler RepHtml
getAdminEditUserHelper userId mErrorMessage = do
    mUser <- runDB $ get userId
    case mUser of
        Nothing -> defaultLayout [whamlet|<h1>User does not exist!|]
        Just user -> do
            userSettings <- fmap (entityVal . fromJust) $
                runDB $ getBy $ UniqueUserSettings userId
            s <- runDB $ selectList [SemesterActive ==. True] []
            p <- runDB $ selectList [PrivilegeUserId ==. userId] []
            let privileges = [ SemesterPriv code name level
                             | Entity sId1 (Semester code name _) <- s
                             , Entity _ (Privilege _ sId2 level) <- p
                             , sId1 == sId2 ]           
            defaultLayout $ do
                setTitle "Edit User"
                $(widgetFile "admin-edit-user")

postAdminEditUserR :: UserId -> Handler RepHtml
postAdminEditUserR userId = do
    (postData, _) <- runRequestBody
    mErrorMessage <- case fromJust $ lookup "type" postData of
        "basic" -> do
            let verified = lookup "verified" postData == Just "yes"
                admin = lookup "admin" postData == Just "yes"
            runDB $ do
                ver <- fmap (userVerified . fromJust) $ get userId
                if (ver && not verified)
                    then return $ Just "Why would you unverify someone?"
                    else do
                        update userId [ UserVerified =. verified
                                      , UserAdmin =. admin]
                        return Nothing
        "privileges" -> do
            let updatePriv (key, value) = do
                    let (priv, code) = T.splitAt 4 key
                    case priv of
                        "priv" -> runDB $ do
                            semId <- fmap (entityKey . fromJust) $
                                            getBy $ UniqueSemester code
                            updateWhere [ PrivilegeUserId ==. userId
                                        , PrivilegeSemester ==. semId ]
                                [PrivilegeLevel =. (read $ T.unpack value)]
                            return ()
                        _ -> return ()
            mapM_ updatePriv postData
            return Nothing
        "delete" -> do
            email <- fmap (userEmail . fromJust) $ runDB $ get userId
            let mEmail = lookup "email" postData
                check1 = lookup "check1" postData == Just "yes"
                check2 = lookup "check2" postData == Just "yes" 
                check3 = lookup "check3" postData == Just "yes"
            if (mEmail == Just email && check1 && check2 && check3)
                then runDB $ do
                    reqIds <- fmap (map entityKey) $
                        selectList [SectionRequestUserId ==. userId] []
                    deleteWhere [NotificationRequestId <-. reqIds]
                    mapM_ delete reqIds
                    deleteWhere [NotificationLogUserId ==. userId]
                    deleteWhere [SmsVerificationUserId ==. userId]
                    deleteWhere [EmailVerificationUserId ==. userId]
                    deleteWhere [PrivilegeUserId ==. userId]
                    deleteWhere [SettingsUserId ==. userId]
                    delete userId
                    return Nothing
                else return $ Just "Delete user: missed some safeguards."
        _ -> return $ Just "unknown type"
    getAdminEditUserHelper userId mErrorMessage

addSemesterForm :: FormInput App App Semester
addSemesterForm = Semester
    <$> ireq textField "Code"
    <*> ireq textField "Name"
    <*> ireq boolField "Active"

getAdminSemestersR :: Handler RepHtml
getAdminSemestersR = do
    semesters <- fmap (map entityVal) $ runDB $ selectList [] []
    let mErrorMessage = Nothing :: Maybe Text
    defaultLayout $ do
        setTitle "Semesters"
        $(widgetFile "admin-semesters")

postAdminSemestersR :: Handler RepHtml
postAdminSemestersR = do
    semester <- runInputPost addSemesterForm
    mErrorMessage <- do
        key <- runDB $ insertBy semester
        case key of
            Left _ -> return $ Just MsgSemesterExists
            Right _ -> return Nothing
    semesters <- fmap (map entityVal) $
                    runDB $ selectList [] [Asc SemesterCode]
    token <- getToken
    defaultLayout $ do
        setTitle "Semesters"
        $(widgetFile "admin-semesters")
