{-# LANGUAGE DoAndIfThenElse #-}
module Handler.Auth where

import Import
import Control.Monad
import Crypto.Scrypt
import Data.Maybe
import qualified Data.Text as T
import Data.Text.Encoding

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
    mUser <- currentUser
    case mUser of
        Just _ -> redirectUltDest SettingsR
        Nothing -> defaultLayout [whamlet|<h1>Not implemented!|]

getResetPasswordR :: Handler RepHtml
getResetPasswordR = do
    mUser <- currentUser
    case mUser of
        Just _ -> redirectUltDest SettingsR
        Nothing -> defaultLayout [whamlet|<h1>Not implemented!|]
