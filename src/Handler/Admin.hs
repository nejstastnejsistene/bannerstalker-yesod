module Handler.Admin where

import Import
import Control.Monad
import Data.Maybe
import qualified Data.Text as T

import Admin

getAdminR :: Handler RepHtml
getAdminR = defaultLayout $ do
    setTitle "Admin"
    $(widgetFile "admin")

adminUsersInfoKey, adminUsersErrorKey :: Text
adminUsersInfoKey = "_AdminUsersR_mInfoMessage"
adminUsersErrorKey = "_AdminUsersR_mInfoMessage"

getAdminUsersR :: Handler RepHtml
getAdminUsersR = do
    users <- runDB $ selectList [] []
    defaultLayout $ do
        setTitle "Users"
        $(widgetFile "admin-users")

adminEditUserInfoKey, adminEditUserErrorKey :: Text
adminEditUserInfoKey = "_AdminEditUserR_mInfoMessage"
adminEditUserErrorKey = "_AdminEditUserR_mErrorMessage"

data SemesterPriv = SemesterPriv Text Text PrivilegeLevel

getAdminEditUserR :: UserId -> Handler RepHtml
getAdminEditUserR userId = do
    mUser <- runDB $ get userId
    case mUser of
        Nothing -> do
            setSession adminUsersErrorKey "User does not exist!"
            redirect AdminUsersR
        Just user -> do
            s <- runDB $ selectList [SemesterActive ==. True] []
            p <- runDB $ selectList [PrivilegeUserId ==. userId] []
            let privileges = [ SemesterPriv code name level
                             | Entity sId1 (Semester code name _) <- s
                             , Entity _ (Privilege _ sId2 level) <- p
                             , sId1 == sId2 ]           
            requests <- runDB $ selectList
                [SectionRequestUserId ==. userId] []
            let reqIds = map entityKey requests :: [SectionRequestId]
                reqVals = map entityVal requests :: [SectionRequest]
                reqMap = [ (sectionRequestSectionId v, k)
                         | Entity k v <- requests ]
            sections <- runDB $ selectList
                [SectionId <-. map sectionRequestSectionId reqVals] []
            notifications <- runDB $ selectList
                [NotificationRequestId <-. reqIds] []
            let sectionsMap = [(k, v) | Entity k v <- sections]
                notificationsMap = [ (notificationRequestId v, v)
                                   | Entity _ v <- notifications ]
            notificationLogs <- fmap (map entityVal) $ runDB $ selectList
                [NotificationLogUserId ==. Just userId]
                [Desc NotificationLogTimestamp]
            mInfoMessage <- getSessionWith adminEditUserInfoKey
            mErrorMessage <- getSessionWith adminEditUserErrorKey
            deleteSession adminEditUserInfoKey
            deleteSession adminEditUserErrorKey
            defaultLayout $ do
                setTitle "Edit User"
                $(widgetFile "admin-edit-user")

postAdminEditUserR :: UserId -> Handler RepHtml
postAdminEditUserR userId = do
    user <- fmap fromJust $ runDB $ get userId
    (postData, _) <- runRequestBody
    case fromJust $ lookup "type" postData of
        "admin" -> do
            runDB $ update userId [UserAdmin =. (not $ userAdmin user)]
            setSession adminEditUserInfoKey $ case not $ userAdmin user of
                True -> "Promoted to admin."
                False -> "Admin revoked."
            redirect $ AdminEditUserR userId
        "privileges" -> do
            let updatePriv (code, value) =
                    when (code /= "type") $ runDB $ do
                        semesterId <- fmap (entityKey . fromJust) $
                            getBy $ UniqueSemester code
                        updateWhere [ PrivilegeUserId ==. userId
                                    , PrivilegeSemester ==. semesterId ]
                                    [ PrivilegeLevel =. (read $ T.unpack value)]
            mapM_ updatePriv postData
            setSession adminEditUserInfoKey "Privileges were updated."
            redirect $ AdminEditUserR userId
        "delete" -> do
            let mEmail = lookup "email" postData
                check1 = lookup "check1" postData == Just "yes"
                check2 = lookup "check2" postData == Just "yes" 
                check3 = lookup "check3" postData == Just "yes"
            if (mEmail == Just (userEmail user) && check1 && check2 && check3)
                then do
                    deleteUser userId
                    setSession adminUsersInfoKey $ T.concat
                        ["User ", userEmail user, " was deleted successfully."]
                    redirect AdminUsersR
                else do
                    setSession adminEditUserErrorKey
                        "Delete user: missed some safeguards."
                    redirect $ AdminEditUserR userId
        "toggle" -> do
            let crn = read $ T.unpack $
                        fromJust $ lookup "crn" postData :: Int
            Entity sectionId section <- fmap fromJust $
                runDB $ getBy $ UniqueCrn crn
            runDB $ updateWhere [SectionCrn ==. crn]
                                [SectionCurrStatus =. Unavailable]
            runDB $ updateWhere [SectionRequestUserId ==. userId
                                ,SectionRequestSectionId ==. sectionId]
                                [SectionRequestLastStatus =.
                                    case sectionCurrStatus section of
                                        Open -> Closed
                                        Closed -> Open
                                        Unavailable -> error "wtf"]
            setSession adminEditUserInfoKey $ "Status toggled... Wait for the daemon's next iteration for a notification to be sent or queued."
            redirect $ AdminEditUserR userId
        action -> do
            setSession adminEditUserErrorKey $ T.concat
                ["Unknown action: ",  action]
            redirect $ AdminEditUserR userId

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
