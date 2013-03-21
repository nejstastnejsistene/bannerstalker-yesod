module Handler.Home where

import Import

getHomeR :: Handler RepHtml
getHomeR = do
    defaultLayout $ do
        setTitle "Bannerstalker"
        $(widgetFile "home")

getInvalidR :: Handler RepHtml
getInvalidR = notFound
