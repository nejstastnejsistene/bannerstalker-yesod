module Handler.Auth where

import Import
import Control.Monad
import Crypto.Scrypt
import qualified Data.Text as T
import Data.Maybe
import Data.Text.Encoding
import qualified Data.Text.Lazy as LT
import Network.Mail.Mime
import System.Random (newStdGen)
import Text.Blaze.Html.Renderer.String
import Text.Hamlet

import Email

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
        Nothing -> defaultLayout [whamlet|<h1>Check your email.|]
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
    userId <- runDB $ do
        userId <- insert $ User
            { userEmail = email
            , userVerified = False
            , userPassword = passwdHash
            , userPrivilege = Level1
            }
        _ <- insert $ Settings
            { settingsUserId = userId
            , settingsPhoneNum = Nothing
            , settingsSmsVerified = False
            , settingsUseEmail = True
            , settingsUseSms = False
            }
        _ <- insert $ EmailVerification
            { emailVerificationUserId = userId
            , emailVerificationVerKey = verKey
            }
        return userId
    render <- getUrlRender
    tm <- getRouteToMaster
    let verUrl = render $ tm $ VerifyEmailR userId verKey
    sendVerificationEmail email verUrl

sendVerificationEmail :: Text -> Text -> Handler ()
sendVerificationEmail email verUrl = liftIO $ do
    message <- simpleMail to from subject text html []
    mySendmail message
    where
        to = Address Nothing email
        from = noreplyAddr
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
    userId <- currentUser
    case userId of
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
    currUserId <- currentUser
    -- If not logged in...
    when (isNothing currUserId) $ do
        -- ...ignore nonexistant users...
        mUser <- runDB $ get userId
        when (isNothing mUser) $ return ()
        -- ...and if the verKey is legit...
        mEmailVer <- runDB $ getBy $ UniqueUserEmailVerification userId
        case mEmailVer of
            Just (Entity evKey (EmailVerification _ correctVerKey)) ->
                when (verKey == correctVerKey) $ do
                    -- ...then verify the user and login.
                    runDB $ do
                        update userId [UserVerified =. True]
                        delete evKey
                    doLogin userId
            Nothing -> return ()
    -- Always redirect to HomeR.
    redirectUltDest HomeR

postResendVerificationEmailR :: Handler RepHtml
postResendVerificationEmailR = do
    defaultLayout [whamlet|<h1>not implemented yet|]

getVerifySmsR :: UserId -> Text -> Handler RepHtml
getVerifySmsR userId verKey =
    defaultLayout [whamlet|not implemented yet|]

postResendVerificationSmsR :: Handler RepHtml
postResendVerificationSmsR =
    defaultLayout [whamlet|not implemented yet|]
