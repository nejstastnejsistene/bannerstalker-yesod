module Handler.Admin where

import Import
import qualified Data.Text as T

getAdminUsers :: Handler RepHtml
getAdminUsers = do
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
            <a href=@{AdminUserEdit userId}>#{userEmail user}
                $if not $ userVerified user
                    \ (unverified)
                $if userAdmin user
                    \ (admin)
|]

getAdminUserEdit :: UserId -> Handler RepHtml
getAdminUserEdit userId = do
    mUser <- runDB $ get userId
    case mUser of
        Nothing -> defaultLayout [whamlet|<h1>User does not exists|]
        Just (User email verified _ admin) -> do
            defaultLayout $ do
                setTitle "Edit user"
                [whamlet|
<ul>
    <li>Email: #{email}
    <li>Verified: #{verified}
    <li>Admin: #{admin}
|]
