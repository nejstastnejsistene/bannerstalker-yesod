{-# LANGUAGE DoAndIfThenElse #-}
module Handler.Auth where

import Import
import Control.Monad
import Crypto.Scrypt
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import Data.Text.Lazy.Builder
import Data.Text.Encoding
import Network.Mail.Mime
import Text.Blaze.Html.Renderer.String
import Text.Hamlet
import Text.Shakespeare.Text

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
registerErrorKey = "_RegisterR_registerError"

getRegisterR :: Handler RepHtml
getRegisterR = do
    -- Redirect to home if already logged in.
    mUser <- currentUser
    when (isJust mUser) $ redirect HomeR
    -- Create form and display page.
    mErrorMessage <- consumeSession registerErrorKey
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
            redirect RegisterR

registerUser :: Text -> Text -> Handler ()
registerUser email passwd = do
    passwdHash <- fmap (decodeUtf8 . unEncryptedPass) $
        liftIO $ encryptPass' $ Pass $ encodeUtf8 passwd
    semesters <- fmap (map entityKey) $ runDB $ selectList [] []
    runDB $ do
        -- Insert the user record.
        userId <- insert $ User
            { userEmail = email
            , userPhoneNum = Nothing
            , userVerified = False
            , userPassword = passwdHash
            , userAdmin = False
            }
        -- Set user to Level1 for all current semesters.
        mapM_ (\x -> insert $ Privilege userId x Level1) semesters
        return ()
    sendVerificationEmail email

loginErrorKey, badLoginCombo :: Text
loginErrorKey = "_LoginR_loginError"
badLoginCombo = "That is not a valid username/password combination."

getLoginR :: Handler RepHtml
getLoginR = do
    -- Redirect to home if already logged in.
    mUser <- currentUser
    when (isJust mUser) $ redirect HomeR
    -- Create form and display page.
    mErrorMessage <- consumeSession loginErrorKey
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
            redirect LoginR

getLogoutR :: Handler RepHtml
getLogoutR = postLogoutR

postLogoutR :: Handler RepHtml
postLogoutR = do
    doLogout
    redirect HomeR

forgotPasswordErrorKey :: Text
forgotPasswordErrorKey = "_ForgotPasswordR_forgotError"

getForgotPasswordR :: Handler RepHtml
getForgotPasswordR = do
    mUser <- currentUser
    case mUser of
        Just _ -> redirect SettingsR
        Nothing -> do
            mErrorMessage <- consumeSession forgotPasswordErrorKey
            defaultLayout $ do
                setTitle "Forgot Password"
                $(widgetFile "forgot-password")

postForgotPasswordR :: Handler RepHtml
postForgotPasswordR = do
    mCurrUser <- currentUser
    case mCurrUser of
        Just _ -> redirect SettingsR
        Nothing -> do
            email <- runInputPost $ ireq emailField "email"
            mUser <- runDB $ getBy $ UniqueEmail email
            case mUser of
                Nothing -> do
                    setSession forgotPasswordErrorKey "Email doesn't exist"
                    redirect ForgotPasswordR
                Just (Entity userId user) -> do
                    render <- getUrlRender
                    tm <- getRouteToMaster
                    let verKey = T.splitOn "|" (userPassword user) !! 4
                        verUrl = render $ tm $ ResetPasswordR userId verKey
                    sendPasswordReset email verUrl
                    redirect ResetSentR

sendPasswordReset :: Text -> Text -> Handler ()
sendPasswordReset email verUrl =
    liftIO $ simpleMail to from subject text html [] >>= mySendmail
    where
        to = Address Nothing email
        from = noreplyAddr
        subject = "Bannerstalker password reset"
        text = toLazyText $ $(textFile "templates/password-reset.text") ()
        html = LT.pack $ renderHtml
                $(shamletFile "templates/password-reset.hamlet")

getResetSentR :: Handler RepHtml
getResetSentR = defaultLayout $ do
    setTitle "Password Reset Sent"
    $(widgetFile "reset-sent")
           
resetPasswordErrorKey :: Text
resetPasswordErrorKey = "_ResetPasswordR_resetError"

getResetPasswordR :: UserId -> Text -> Handler RepHtml
getResetPasswordR userId verKey = do
    mCurrUser <- currentUser
    case mCurrUser of
        Just _ -> redirect SettingsR
        Nothing -> do
            mUser <- confirmPasswdHash userId verKey
            case mUser of
                Nothing -> expiredResetLink
                Just (User email _ _ _ _) -> do
                    mErrorMessage <- consumeSession resetPasswordErrorKey
                    defaultLayout $ do
                        setTitle "Reset Password"
                        $(widgetFile "reset-password")

postResetPasswordR :: UserId -> Text -> Handler RepHtml
postResetPasswordR userId verKey = do
    mCurrUser <- currentUser
    case mCurrUser of
        Just _ -> redirect SettingsR
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
                        redirect HomeR
                    else do
                        setSession resetPasswordErrorKey passwordMismatch
                        redirect $ ResetPasswordR userId verKey

confirmPasswdHash :: UserId -> Text -> Handler (Maybe User)
confirmPasswdHash userId verKey= do
    mUser <- runDB $ get userId
    case mUser of
        Nothing -> return Nothing
        Just user -> 
            if verKey == T.splitOn "|" (userPassword user) !! 4 then
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
$newline never
<h3>This link is expired
<p>
    Request another one
    <a href=@{ForgotPasswordR}>here.
|]
