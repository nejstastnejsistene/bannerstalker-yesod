module Handler.Admin where

import Import
import Data.Maybe

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
    let mErrorMessage = Just "Not implemented yet!"
    getAdminEditUserHelper userId mErrorMessage

addSemesterForm :: Form Semester
addSemesterForm = renderDivs $ Semester
    <$> areq textField "Code" Nothing
    <*> areq textField "Name" Nothing
    <*> areq boolField "Active" Nothing

getAdminSemestersR :: Handler RepHtml
getAdminSemestersR = do
    (widget, enctype) <- generateFormPost addSemesterForm
    semesters <- fmap (map entityVal) $ runDB $ selectList [] []
    let mErrorMessage = Nothing :: Maybe Text
    defaultLayout $ do
        setTitle "Semesters"
        $(widgetFile "admin-semesters")

postAdminSemestersR :: Handler RepHtml
postAdminSemestersR = do
    ((result, widget), enctype) <- runFormPost addSemesterForm
    mErrorMessage <- case result of
        FormSuccess semester -> do
            key <- runDB $ insertBy semester
            case key of
                Left _ -> return $ Just MsgSemesterExists
                Right _ -> return Nothing
        _ -> return $ Just MsgFormError
    semesters <- fmap (map entityVal) $
                    runDB $ selectList [] [Asc SemesterCode]
    defaultLayout $ do
        setTitle "Semesters"
        $(widgetFile "admin-semesters")
