module Handler.Upgrade where

import Import

import Stripe

getUpgradeR :: Handler RepHtml
getUpgradeR = defaultLayout $ do
    setTitle "Upgrade"
    $(widgetFile "upgrade")

postUpgradeR :: Handler RepHtml
postUpgradeR = do
    stripeToken <- runInputPost $ ireq textField "stripeToken"
    manager <- fmap httpManager getYesod
    extra <- getExtra
    asdf <- liftIO $ makeCharge manager extra stripeToken "1000" "test"
    defaultLayout $ do
        setTitle "Payment Successful"
        [whamlet|
$case asdf
    $of Left a
        <p>#{show a}
    $of Right b
        <p>#{show b}
|]
