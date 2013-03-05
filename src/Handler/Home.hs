{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Home where

import Import

getHomeR :: Handler RepHtml
getHomeR = do
    mUserId <- currentUserId
    mUser <- case mUserId of
        Nothing -> return Nothing
        Just userId -> runDB $ get userId
    defaultLayout $ do
        setTitle "Bannerstalker"
        $(widgetFile "home")

getQuickstartR :: Handler RepHtml
getQuickstartR = defaultLayout $ do
    setTitle "Quickstart"
    $(widgetFile "quickstart")
