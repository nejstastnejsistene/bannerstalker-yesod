{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Home where

import Import

import Handler.Auth

getHomeR :: Handler RepHtml
getHomeR = do
    mUser <- currentUser
    (loginWidget, lEnctype) <- generateFormPost loginForm
    (registerWidget, rEnctype) <- generateFormPost registerForm
    defaultLayout $ do
        setTitle "Bannerstalker"
        $(widgetFile "home")
