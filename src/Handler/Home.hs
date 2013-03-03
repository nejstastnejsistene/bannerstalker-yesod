{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Home where

import Import

getHomeR :: Handler RepHtml
getHomeR = do
    mUserId <- currentUser
    mUser <- case mUserId of
        Nothing -> return Nothing
        Just userId -> runDB $ get userId
    case mUser of
        Just user -> defaultLayout [whamlet|
<h1>Welcome, #{userEmail user}
<a href=@{LogoutR}>Logout|]
        Nothing -> defaultLayout [whamlet|
<h1>not logged in
<a href=@{RegisterR}>Register
<a href=@{LoginR}>Login
|]

{-
postHomeR :: Handler RepHtml
postHomeR = do
    ((result, formWidget), formEnctype) <- runFormPost sampleForm
    let handlerName = "postHomeR" :: Text
        submission = case result of
            FormSuccess res -> Just res
            _ -> Nothing

    defaultLayout $ do
        aDomId <- lift newIdent
        setTitle "Welcome To Yesod!"
        $(widgetFile "homepage")

sampleForm :: Form (FileInfo, Text)
sampleForm = renderDivs $ (,)
    <$> fileAFormReq "Choose a file"
    <*> areq textField "What's on the file?" Nothing
-}
