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
