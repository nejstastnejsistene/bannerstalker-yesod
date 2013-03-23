module Handler.Home where

import Import

getHomeR :: Handler RepHtml
getHomeR = do
    mUser <- currentUser
    defaultLayout $ do
        setTitle "Bannerstalker"
        $(widgetFile "home")

getTermsOfUseR :: Handler RepHtml
getTermsOfUseR = defaultLayout $ do
    setTitle "Terms and Conditions of Use"
    $(widgetFile "tos")

getInvalidR :: Handler RepHtml
getInvalidR = notFound
