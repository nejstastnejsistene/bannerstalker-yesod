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
    -- Redirect to home if already logged in.
    userId <- currentUserId
    when (isJust userId) $ redirectUltDest HomeR
    -- Create form and display page.
    (widget, enctype) <- generateFormPost registerForm
    let mErrorMessage = Nothing :: Maybe Text
    defaultLayout $ do
        setTitle "Register"
        $(widgetFile "register")

postRegisterR :: Handler RepHtml
postRegisterR = do
    ((result, widget), enctype) <- runFormPost registerForm
    mErrorMessage <- case result of
        FormSuccess (RegisterCreds email passwd confirm) -> do
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
        -- Form error.
        _ -> return $ Just MsgFormError
    case mErrorMessage of
        Nothing -> redirectUltDest VerificationSentR
        _ -> defaultLayout $ do
            setTitle "Register"
            $(widgetFile "register")

registerUser :: Text -> Text -> Handler ()
registerUser email passwd = do
    passwdHash <- fmap (decodeUtf8 . unEncryptedPass) $
        liftIO $ encryptPass' $ Pass $ encodeUtf8 passwd
    -- Insert User and Settings records.
    runDB $ do
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
        return ()
    sendVerificationEmail email

getLoginR :: Handler RepHtml
getLoginR = do
    -- Redirect to home if already logged in.
    userId <- currentUserId
    when (isJust userId) $ redirectUltDest HomeR
    -- Create form and display page.
    (widget, enctype) <- generateFormPost loginForm
    let mErrorMessage = Nothing :: Maybe Text
    defaultLayout $ do
        setTitle "Login"
        $(widgetFile "login")

postLoginR :: Handler RepHtml
postLoginR = do
    ((result, widget), enctype) <- runFormPost loginForm
    mErrorMessage <- case result of
        FormSuccess (LoginCreds email passwd) -> do
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
        -- Form error.
        _ -> return $ Just MsgFormError
    case mErrorMessage of
        Nothing -> redirectUltDest VerificationSentR
        _ -> defaultLayout $ do
            setTitle "Login"
            $(widgetFile "login")

getLogoutR :: Handler RepHtml
getLogoutR = postLogoutR

postLogoutR :: Handler RepHtml
postLogoutR = do
    doLogout
    redirectUltDest HomeR
