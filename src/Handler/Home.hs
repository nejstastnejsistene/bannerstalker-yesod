module Handler.Home where

import Import

getHomeR :: Handler RepHtml
getHomeR = do
    mUser <- currentUser
    defaultLayout $ do
        setTitle "Bannerstalker"
        $(widgetFile "home")

getInvalidR :: Handler RepHtml
getInvalidR = notFound
