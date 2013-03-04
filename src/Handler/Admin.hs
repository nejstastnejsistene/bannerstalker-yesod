module Handler.Admin where

import Import

getAdminUsersR :: Handler RepHtml
getAdminUsersR = do
    users <- runDB $ selectList [] []
    defaultLayout $ do
        setTitle "Users"
        [whamlet|
$if length users == 1
    <p>1 user.
$else
    <p>#{length users} users.
<ul>
    $forall Entity userId user <- users
        <li>
            <a href=@{AdminUserEditR userId}>#{userEmail user}
                $if not $ userVerified user
                    \ (unverified)
                $if userAdmin user
                    \ (admin)
|]

data UserRecord = UserRecord Bool Text Text

editUserForm :: Form UserRecord
editUserForm = renderDivs $ UserRecord
    <$> areq boolField "Email verified" Nothing
    <*> areq passwordField "Password" Nothing
    <*> areq passwordField "Confirm password" Nothing

getAdminUserEditR :: UserId -> Handler RepHtml
getAdminUserEditR userId = do
    mUser <- runDB $ get userId
    case mUser of
        Nothing -> defaultLayout [whamlet|<h1>User does not exists|]
        Just (User email verified _ admin) -> do
            (widget, enctype) <- generateFormPost editUserForm
            defaultLayout $ do
                setTitle "Edit user"
                [whamlet|
<h2>#{email}
<form method=post action=@{AdminUserEditR userId} enctype=#{enctype}>
    ^{widget}
    <input type=submit>
|]

postAdminUserEditR :: UserId -> Handler RepHtml
postAdminUserEditR userId = defaultLayout [whamlet|<h1>not implemented|]

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
            result <- runDB $ insertBy semester
            case result of
                Left _ -> return $ Just MsgSemesterExists
                Right _ -> return Nothing
        _ -> return $ Just MsgFormError
    semesters <- fmap (map entityVal) $ runDB $ selectList [] []
    defaultLayout $ do
        setTitle "Semesters"
        $(widgetFile "admin-semesters")
