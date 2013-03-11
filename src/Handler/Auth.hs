{-# LANGUAGE DoAndIfThenElse #-}
module Handler.Auth where

import Import
import Control.Monad
import Crypto.Scrypt
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import Data.Text.Encoding
import Network.Mail.Mime
import Text.Blaze.Html.Renderer.String
import Text.Hamlet

import Email
import Handler.Verify

data LoginCreds = LoginCreds Text Text
data RegisterCreds = RegisterCreds Text Text Text

loginForm :: FormInput App App LoginCreds
loginForm = LoginCreds
    <$> ireq emailField "email"
    <*> ireq passwordField "password"

registerForm :: FormInput App App RegisterCreds
registerForm = RegisterCreds
    <$> ireq emailField "email"
    <*> ireq passwordField "password"
    <*> ireq passwordField "confirm"

registerErrorKey :: Text
registerErrorKey = "_RegisterR_mErrorMessage"

getRegisterR :: Handler RepHtml
getRegisterR = do
    -- Redirect to home if already logged in.
    mUser <- currentUser
    when (isJust mUser) $ redirectUltDest HomeR
    -- Create form and display page.
    mErrorMessage <- getSessionWith registerErrorKey
    deleteSession registerErrorKey
    token <- getToken
    defaultLayout $ do
        setTitle "Register"
        $(widgetFile "register")

postRegisterR :: Handler RepHtml
postRegisterR = do
    RegisterCreds email passwd confirm <- runInputPost registerForm
    mErrorMessage <- do
        mUser <- runDB $ getBy $ UniqueEmail email
        -- Only @email.wm.edu students may register.
        if (snd $ T.breakOn "@" email) /= "@email.wm.edu" then
            return $ Just "W&M students only"
        -- Already registered.
        else if (isJust mUser) then
            return $ Just "Already registered"
        -- Password mismatch.
        else if passwd /= confirm then
            return $ Just passwordMismatch
        -- Password too short.
        else if T.length passwd < 8 then
            return $ Just passwordTooShort
        -- Success!
        else registerUser email passwd >> return Nothing
    token <- getToken
    case mErrorMessage of
        Nothing -> defaultLayout $ do
            setTitle "Verify your email"
            $(widgetFile "verification-sent")
        _ -> do
            setSessionWith registerErrorKey mErrorMessage
            redirectUltDest RegisterR

registerUser :: Text -> Text -> Handler ()
registerUser email passwd = do
    passwdHash <- fmap (decodeUtf8 . unEncryptedPass) $
        liftIO $ encryptPass' $ Pass $ encodeUtf8 passwd
    semesters <- fmap (map entityKey) $ runDB $ selectList [] []
    runDB $ do
        -- Insert the user record.
        userId <- insert $ User
            { userEmail = email
            , userVerified = False
            , userPassword = passwdHash
            , userAdmin = False
            }
        -- Set user to Level1 for all current semesters.
        mapM_ (\x -> insert $ Privilege userId x Level1) semesters
        -- Insert default user settings.
        _ <- insert $ Settings
            { settingsUserId = userId
            , settingsPhoneNum = Nothing
            , settingsSmsVerified = False
            , settingsUseEmail = True
            , settingsUseSms = False
            }
        return ()
    sendVerificationEmail email

loginErrorKey, badLoginCombo :: Text
loginErrorKey = "_LoginR_mErrorMessage"
badLoginCombo = "That is not a valid username/password combination."

getLoginR :: Handler RepHtml
getLoginR = do
    -- Redirect to home if already logged in.
    mUser <- currentUser
    when (isJust mUser) $ redirectUltDest HomeR
    -- Create form and display page.
    mErrorMessage <- getSessionWith loginErrorKey
    deleteSession loginErrorKey
    defaultLayout $ do
        setTitle "Login"
        $(widgetFile "login")

postLoginR :: Handler RepHtml
postLoginR = do
    LoginCreds email passwd <- runInputPost loginForm
    mErrorMessage <- do
        mUser <- runDB $ getBy $ UniqueEmail $ email
        case mUser of
            -- User does not exists with this email.
            Nothing -> return $ Just badLoginCombo
            Just (Entity userId user) -> do
                -- Compare password hashes.
                let pass = Pass $ encodeUtf8 passwd
                    hash = EncryptedPass $
                                encodeUtf8 $ userPassword user
                if (userVerified user) then
                    if (verifyPass' pass hash) then
                        doLogin userId >> return Nothing
                    -- Passwords don't match.
                    else return $ Just badLoginCombo
                else return $ Just "you're account isn't verified yet, [here] is a link..."
    case mErrorMessage of
        Nothing -> redirectUltDest HomeR
        _ -> do
            setSessionWith loginErrorKey mErrorMessage
            redirectUltDest LoginR

getLogoutR :: Handler RepHtml
getLogoutR = postLogoutR

postLogoutR :: Handler RepHtml
postLogoutR = do
    doLogout
    redirectUltDest HomeR

forgotPasswordErrorKey :: Text
forgotPasswordErrorKey = "_ForgotPasswordR_mErrorMessage"

getForgotPasswordR :: Handler RepHtml
getForgotPasswordR = do
    mUser <- currentUser
    case mUser of
        Just _ -> redirectUltDest SettingsR
        Nothing -> do
            mErrorMessage <- getSessionWith forgotPasswordErrorKey
            deleteSession forgotPasswordErrorKey
            defaultLayout $ do
                setTitle "Forgot Password"
                $(widgetFile "forgot-password")

postForgotPasswordR :: Handler RepHtml
postForgotPasswordR = do
    mCurrUser <- currentUser
    case mCurrUser of
        Just _ -> redirectUltDest SettingsR
        Nothing -> do
            email <- runInputPost $ ireq emailField "email"
            mUser <- runDB $ getBy $ UniqueEmail email
            case mUser of
                Nothing -> do
                    setSession forgotPasswordErrorKey "Email doesn't exist"
                    redirectUltDest ForgotPasswordR
                Just (Entity userId (User _ _ passwd _)) -> do
                    render <- getUrlRender
                    tm <- getRouteToMaster
                    let verKey = T.splitOn "|" passwd !! 4
                        verUrl = render $ tm $ ResetPasswordR userId verKey
                    sendPasswordReset email verUrl
                    redirectUltDest ResetSentR

sendPasswordReset :: Text -> Text -> Handler ()
sendPasswordReset email verUrl =
    liftIO $ simpleMail to from subject text html [] >>= mySendmail
    where
        to = Address Nothing email
        from = noreplyAddr
        subject = "Bannerstalker password reset"
        text = LT.pack $ renderHtml
                $(shamletFile "templates/password-reset/mail-text.hamlet")
        html = LT.pack $ renderHtml
                $(shamletFile "templates/password-reset/mail-html.hamlet")

getResetSentR :: Handler RepHtml
getResetSentR = defaultLayout $ do
    setTitle "Password Reset Sent"
    $(widgetFile "reset-sent")
           
resetPasswordErrorKey :: Text
resetPasswordErrorKey = "_ResetPasswordR_mErrorMessage"

getResetPasswordR :: UserId -> Text -> Handler RepHtml
getResetPasswordR userId verKey = do
    mCurrUser <- currentUser
    case mCurrUser of
        Just _ -> redirectUltDest SettingsR
        Nothing -> do
            mUser <- confirmPasswdHash userId verKey
            case mUser of
                Nothing -> expiredResetLink
                Just (User email _ _ _) -> do
                    mErrorMessage <- getSessionWith resetPasswordErrorKey
                    deleteSession resetPasswordErrorKey
                    defaultLayout $ do
                        setTitle "Reset Password"
                        $(widgetFile "reset-password")

postResetPasswordR :: UserId -> Text -> Handler RepHtml
postResetPasswordR userId verKey = do
    mCurrUser <- currentUser
    case mCurrUser of
        Just _ -> redirectUltDest SettingsR
        Nothing -> do
            mUser <- confirmPasswdHash userId verKey
            case mUser of
                Nothing -> expiredResetLink
                Just _ -> do
                    (passwd, confirm) <- runInputPost $ (,)
                        <$> ireq passwordField "password"
                        <*> ireq passwordField "confirm"
                    if passwd == confirm then do
                        changePassword userId passwd
                        doLogin userId
                        redirectUltDest HomeR
                    else do
                        setSession resetPasswordErrorKey passwordMismatch
                        redirectUltDest $ ResetPasswordR userId verKey

confirmPasswdHash :: UserId -> Text -> Handler (Maybe User)
confirmPasswdHash userId verKey= do
    mUser <- runDB $ get userId
    case mUser of
        Nothing -> return Nothing
        Just user@(User _ _ passwd _) -> 
            if verKey == T.splitOn "|" passwd !! 4 then
                return $ Just user
            else return Nothing

changePassword :: UserId -> Text -> Handler ()
changePassword userId passwd = do
    passwdHash <- fmap (decodeUtf8 . unEncryptedPass) $
        liftIO $ encryptPass' $ Pass $ encodeUtf8 passwd
    runDB $ update userId [UserPassword =. passwdHash]
    return ()

expiredResetLink :: Handler RepHtml
expiredResetLink = defaultLayout [whamlet|
<h3>This link is expired
<p>
    Request another one
    <a href=@{ForgotPasswordR}>here.
|]
