module Handler.Auth where

import Import
import Crypto.Scrypt
import qualified Data.Text as T
import Data.Text.Encoding
import qualified Data.Text.Lazy as LT
import Network.Mail.Mime
import System.Random (newStdGen)
import Text.Blaze.Html.Renderer.String
import Text.Hamlet

data LoginCreds = LoginCreds Text Text
data RegisterCreds = RegisterCreds Text Text Text

loginForm :: Form LoginCreds
loginForm = renderDivs $ LoginCreds
    <$> areq emailField "Email" Nothing
    <*> areq passwordField "Password" Nothing

registerForm :: Form RegisterCreds
registerForm = renderDivs $ RegisterCreds
    <$> areq emailField "Email" Nothing
    <*> areq passwordField "Password" Nothing
    <*> areq passwordField "Confirm Password" Nothing

getRegisterR :: Handler RepHtml
getRegisterR = do
    user <- currentUser
    case user of
        Just _ -> redirectUltDest HomeR
        Nothing -> do
            (widget, enctype) <- generateFormPost registerForm
            defaultLayout $ do
                setTitle "Register"
                [whamlet|
<form method=post action=@{RegisterR} enctype=#{enctype}>
    ^{widget}
    <input type=submit>
|]

postRegisterR :: Handler RepHtml
postRegisterR = do
    ((result, widget), enctype) <- runFormPost registerForm
    mError <- case result of
        FormSuccess (RegisterCreds email passwd confirm) -> 
            -- Only @email.wm.edu students may register.
            if (snd $ T.breakOn "@" email) /= "@email.wm.edu" then
                return $ Just [whamlet|
                    <p> Only W&M students with an email.wm.edu address may
                        \ register.|]
            else do
                muser <- runDB $ getBy $ UniqueEmail email
                case muser of
                    -- Already registered.
                    Just (Entity _ user) -> do
                        let alreadyRegistered = [whamlet|
                                <p> Someone is already registered with this
                                    \ email address.|]
                        if userVerified user
                            then return $ Just alreadyRegistered
                            else return $ Just [whamlet|
                                ^{alreadyRegistered}
                                -- TODO: change this link.
                                <a href=@{HomeR}>Resend Verification|]
                    _ ->
                        -- Password mismatch.
                        if passwd /= confirm then
                            return $ Just [whamlet|
                                <p> Passwords do not match|]
                        -- Password too short.
                        else if T.length passwd < 8 then
                            return $ Just [whamlet|
                                <p> Passwords must be at least 8
                                    \ characters.|]
                        -- Success!
                        else do
                            registerUser email passwd
                            return Nothing
        -- Form error.
        _ -> return $ Just [whamlet|<p>Form error. Please try again.|]
    case mError of
        Nothing -> defaultLayout [whamlet|<h1>Check your email, bitch.|]
        Just errHtml -> defaultLayout [whamlet|
<div style="color:red">
    ^{errHtml}
<form method=post action=@{RegisterR} enctype=#{enctype}>
    ^{widget}
    <input type=submit>
|]

registerUser :: Text -> Text -> Handler ()
registerUser email passwd = do
    stdgen <- liftIO newStdGen
    let verKey = T.pack $ fst $ randomString 20 stdgen
    passwdHash <- fmap (decodeUtf8 . unEncryptedPass) $
        liftIO $ encryptPass' $ Pass $ encodeUtf8 passwd
    userId <- runDB $ insert $ User
        { userEmail = email
        , userPassword = passwdHash
        , userPrivilege = Level1
        , userVerkey = Just verKey
        , userVerified = False
        , userPhoneNum = Nothing
        , userUseEmail = True
        , userUseSms = False
        }
    render <- getUrlRender
    tm <- getRouteToMaster
    let verUrl = render $ tm $ VerifyEmailR userId verKey
    sendVerificationEmail email verUrl

sendVerificationEmail :: Text -> Text -> Handler ()
sendVerificationEmail email verUrl = liftIO $ do
    message <- simpleMail to from subject text html []
    renderSendMail message
    where
        to = Address Nothing email
        from = Address (Just "Bannerstalker") "info@bannerstalker.com"
        subject = "Verify your email address"
        text = LT.pack $ renderHtml [shamlet|
Please confirm your email address by clicking on the link below.

\#{verUrl}

Thank you
|]
        html = LT.pack $ renderHtml [shamlet|
<p>Please confirm your email address by clicking on the link below.
<p>
    <a href=#{verUrl}>#{verUrl}
<p>Thank you
|]

getLoginR :: Handler RepHtml
getLoginR = do
    user <- currentUser
    case user of
        Just _ -> redirectUltDest HomeR
        Nothing -> do
            (widget, enctype) <- generateFormPost loginForm
            defaultLayout $ do
                setTitle "Login"
                [whamlet|
<form method=post action=@{LoginR} enctype=#{enctype}>
    ^{widget}
    <input type=submit>
|]

postLoginR :: Handler RepHtml
postLoginR = do
    ((result, widget), enctype) <- runFormPost loginForm
    mError <- case result of
        FormSuccess (LoginCreds email passwd) -> do
            muser <- runDB $ getBy $ UniqueEmail $ email
            case muser of
                Nothing -> return $ Just loginError
                Just (Entity userId user) -> do
                    let pass = Pass $ encodeUtf8 passwd
                        hash = EncryptedPass $
                                    encodeUtf8 $ userPassword user
                    if (userVerified user && verifyPass' pass hash)
                        then doLogin userId >> return Nothing
                        else return $ Just loginError
        _ -> return $ Just [whamlet|<p>form error|]
    case mError of
        Nothing -> redirectUltDest HomeR
        Just errHtml -> defaultLayout [whamlet|
<div style="color:red">
    ^{errHtml}
<form method=post action=@{LoginR} enctype=#{enctype}>
    ^{widget}
    <input type=submit>
|]
    where
        loginError = [whamlet|Invalid email/password combo.|]

getLogoutR :: Handler RepHtml
getLogoutR = postLogoutR

postLogoutR :: Handler RepHtml
postLogoutR = do
    doLogout
    redirectUltDest HomeR

getVerifyEmailR :: UserId -> Text -> Handler RepHtml
getVerifyEmailR userId verKey = do
    currUser <- currentUser
    case currUser of
        -- Already logged in.
        Just _ -> redirectUltDest HomeR
        -- Not logged in, check verification.
        Nothing -> do
            muser <- runDB $ get userId
            case muser of
                Just user -> if userVerkey user == Just verKey
                    then do
                        -- Verify and login user.
                        runDB $ update userId [ UserVerkey =. Nothing
                                              , UserVerified =. True ]
                        doLogin userId
                        redirectUltDest HomeR
                    -- Ignore bad key.
                    else redirectUltDest HomeR
                -- Ignore nonexistant user.
                Nothing -> redirectUltDest HomeR

postResendVerificationEmailR :: Handler RepHtml
postResendVerificationEmailR = do
    defaultLayout [whamlet|<h1>not implemented yet|]

getVerifySmsR :: UserId -> Text -> Handler RepHtml
getVerifySmsR userId verKey =
    defaultLayout [whamlet|not implemented yet|]

postResendVerificationSmsR :: Handler RepHtml
postResendVerificationSmsR =
    defaultLayout [whamlet|not implemented yet|]
