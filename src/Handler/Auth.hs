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

getRegisterR :: Handler RepHtml
getRegisterR = do
    -- Redirect to home if already logged in.
    mUser <- currentUser
    when (isJust mUser) $ redirectUltDest HomeR
    -- Create form and display page.
    let mErrorMessage = Nothing :: Maybe Text
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
            return $ Just MsgWMStudentsOnly
        -- Already registered.
        else if (isJust mUser) then
            return $ Just MsgAlreadyRegistered
        -- Password mismatch.
        else if passwd /= confirm then
            return $ Just MsgPasswordMismatch
        -- Password too short.
        else if T.length passwd < 8 then
            return $ Just MsgPasswordTooShort
        -- Success!
        else registerUser email passwd >> return Nothing
    token <- getToken
    case mErrorMessage of
        Nothing -> defaultLayout $ do
            setTitle "Verify your email"
            $(widgetFile "verification-sent")
        _ -> defaultLayout $ do
            setTitle "Register"
            $(widgetFile "register")

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

getLoginR :: Handler RepHtml
getLoginR = do
    -- Redirect to home if already logged in.
    mUser <- currentUser
    when (isJust mUser) $ redirectUltDest HomeR
    -- Create form and display page.
    let mErrorMessage = Nothing :: Maybe Text
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
            Nothing -> return $ Just MsgLoginError
            Just (Entity userId user) -> do
                -- Compare password hashes.
                let pass = Pass $ encodeUtf8 passwd
                    hash = EncryptedPass $
                                encodeUtf8 $ userPassword user
                if (userVerified user && verifyPass' pass hash)
                    then doLogin userId >> return Nothing
                    -- Passwords don't match.
                    else return $ Just MsgLoginError
    case mErrorMessage of
        Nothing -> redirectUltDest HomeR
        _ -> defaultLayout $ do
            setTitle "Login"
            $(widgetFile "login")

getLogoutR :: Handler RepHtml
getLogoutR = postLogoutR

postLogoutR :: Handler RepHtml
postLogoutR = do
    doLogout
    redirectUltDest HomeR

getForgotPasswordR :: Handler RepHtml
getForgotPasswordR = do
    let mErrorMessage = Nothing :: Maybe Text
    mUser <- currentUser
    case mUser of
        Just _ -> redirectUltDest SettingsR
        Nothing -> defaultLayout $ do
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
                    let mErrorMessage = Just MsgEmailDoesntExist
                    defaultLayout $ do
                        setTitle "Forgot Password"
                        $(widgetFile "forgot-password")
                Just (Entity userId (User _ _ passwd _)) -> do
                    render <- getUrlRender
                    tm <- getRouteToMaster
                    let verKey = T.splitOn "|" passwd !! 4
                        verUrl = render $ tm $ ResetPasswordR userId verKey
                    sendPasswordReset email verUrl
                    defaultLayout $ do
                        setTitle "Password Reset Sent"
                        $(widgetFile "reset-sent")

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
           
getResetPasswordR :: UserId -> Text -> Handler RepHtml
getResetPasswordR userId verKey = do
    mCurrUser <- currentUser
    case mCurrUser of
        Just _ -> redirectUltDest SettingsR
        Nothing -> do
            mUser <- runDB $ get userId
            case mUser of
                Nothing -> redirectUltDest SettingsR
                Just (User email _ passwd _) ->
                    if verKey == T.splitOn "|" passwd !! 4 then
                        defaultLayout $ do
                            setTitle "Reset Password"
                            $(widgetFile "reset-password")
                    else defaultLayout [whamlet|
<h3>This link is expired
<p .lead>
    Request another one
    <a href=@{ForgotPasswordR}>here.
|]
