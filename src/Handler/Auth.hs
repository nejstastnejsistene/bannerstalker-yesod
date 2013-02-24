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

data RegistrationResult = NotWmStudent
                        | PasswordMismatch
                        | AlreadyRegistered
                        | RegistrationSuccessful

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
    ((result, _), _) <- runFormPost registerForm
    case result of
        FormSuccess creds -> do
            err <- attemptRegistration creds            
            defaultLayout $ case err of
                NotWmStudent -> [whamlet|<h1>not wm student|]
                PasswordMismatch -> [whamlet|<h1>password mismatch|]
                AlreadyRegistered -> [whamlet|
<h1>already registered
<p>plus resend verification
|]
                RegistrationSuccessful -> [whamlet|<h1>success|]
        _ -> defaultLayout [whamlet|form error|]    

attemptRegistration :: RegisterCreds -> Handler RegistrationResult
attemptRegistration (RegisterCreds email passwd confirm) =
    if (snd $ T.breakOn "@" email) /= "@email.wm.edu"
        then return NotWmStudent
    else if passwd /= confirm
        then return PasswordMismatch
    else do
        muser <- runDB $ getBy $ UniqueEmail email
        case muser of
            Just _ -> return AlreadyRegistered
            _ -> registerUser email passwd >> return RegistrationSuccessful

registerUser :: Text -> Text -> Handler ()
registerUser email passwd = do
    stdgen <- liftIO newStdGen
    let verKey = T.pack $ fst $ randomString 20 stdgen
        pass = Pass $ encodeUtf8 passwd
    passwdHash <- liftIO $ encryptPass' pass
    userId <- runDB $ insert $ User
        { userEmail = email
        , userPassword = decodeUtf8 $ unEncryptedPass passwdHash
        , userPrivilege = Level1
        , userVerkey = Just verKey
        , userVerified = False
        , userPhoneNum = Nothing
        , userUseEmail = True
        , userUseSms = False
        }
    render <- getUrlRender
    tm <- getRouteToMaster
    let verUrl = render $ tm $ VerifyR userId verKey
    sendVerifyEmail email verUrl

sendVerifyEmail :: Text -> Text -> Handler ()
sendVerifyEmail email verUrl = liftIO $ do
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

getVerifyR :: UserId -> Text -> Handler RepHtml
getVerifyR _ _ = defaultLayout [whamlet|<h1>not implemented|]

getLoginR :: Handler RepHtml
getLoginR = defaultLayout [whamlet|<h1>not implemented|]

postLoginR :: Handler RepHtml
postLoginR = defaultLayout [whamlet|<h1>not implemented|]

getLogoutR :: Handler RepHtml
getLogoutR = setUltDestReferer >> postLogoutR

postLogoutR :: Handler RepHtml
postLogoutR = do
    deleteSession "_ID"
    redirectUltDest HomeR
