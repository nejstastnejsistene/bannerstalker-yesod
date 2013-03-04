{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Home where

import Import

getHomeR :: Handler RepHtml
getHomeR = do
    mUserId <- currentUserId
    mUser <- case mUserId of
        Nothing -> return Nothing
        Just userId -> runDB $ get userId
    case mUser of
        Just user -> defaultLayout [whamlet|
<h1>Welcome, #{userEmail user}
<a href=@{QuickstartR}>Quickstart
<a href=@{LogoutR}>Logout
|]
        Nothing -> defaultLayout [whamlet|
<h1>not logged in
<a href=@{RegisterR}>Register
<a href=@{LoginR}>Login
|]

getQuickstartR :: Handler RepHtml
getQuickstartR = defaultLayout $ do
    setTitle "Quickstart"
    $(widgetFile "quickstart")
