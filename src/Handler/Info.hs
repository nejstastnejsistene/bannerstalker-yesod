module Handler.Info where

import Import

getAboutR :: Handler RepHtml
getAboutR = defaultLayout $ do
    setTitle "About"
    $(widgetFile "about")

getPricingR :: Handler RepHtml
getPricingR = defaultLayout $ do
    setTitle "Pricing"
    $(widgetFile "pricing")
        
