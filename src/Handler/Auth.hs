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

credsKey :: Text
credsKey = "_ID"

doLogin :: UserId -> Handler ()
doLogin userId = setSession credsKey $ toPathPiece userId

doLogout :: Handler ()
doLogout = deleteSession credsKey

currentUser :: Handler (Maybe User)
currentUser = do
    muserId <- lookupSession credsKey
    case muserId of
        -- Not logged in.
        Nothing -> return Nothing
        Just suserId -> do
            case fromPathPiece suserId of
                -- Invalid userId.
                Nothing -> do
                    doLogout
                    return Nothing
                Just userId -> do
                    muser <- runDB $ get userId
                    case muser of
                        -- User does not exists.
                        Nothing -> do
                            doLogout
                            return Nothing
                        _ -> return muser

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
    else do
        muser <- runDB $ getBy $ UniqueEmail email
        case muser of
            Just _ -> return AlreadyRegistered
            _ -> if passwd /= confirm then
                    return PasswordMismatch
                 else do
                    registerUser email passwd
                    return RegistrationSuccessful

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
getVerifyR userId verKey = do
    muser <- runDB $ get userId
    case muser of
        Just user -> if userVerkey user == Just verKey
            then do
                runDB $ update userId [ UserVerkey =. Nothing
                                      , UserVerified =. True ]
                doLogin userId
                redirectUltDest HomeR
            else keyError
        Nothing -> keyError
    where
        keyError = defaultLayout [whamlet|<h1>invalid key|]

getLoginR :: Handler RepHtml
getLoginR = do
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
    ((result, _), _) <- runFormPost loginForm
    case result of
        FormSuccess (LoginCreds email passwd) -> do
            muser <- runDB $ getBy $ UniqueEmail $ email
            case muser of
                Nothing -> loginError
                Just (Entity userId user) -> do
                    let pass = Pass $ encodeUtf8 passwd
                        hash = EncryptedPass $
                                    encodeUtf8 $ userPassword user
                    if (userVerified user && verifyPass' pass hash)
                        then do
                            doLogin userId
                            redirectUltDest HomeR
                        else loginError
        _ -> defaultLayout [whamlet|form error|]
    where
        loginError = defaultLayout [whamlet|invalid combo|]

getLogoutR :: Handler RepHtml
getLogoutR = postLogoutR

postLogoutR :: Handler RepHtml
postLogoutR = do
    doLogout
    redirectUltDest HomeR

getCheckR :: Handler RepHtml
getCheckR = do
    muser <- currentUser
    case muser of
        Nothing -> defaultLayout [whamlet|<h1>not logged in|]
        Just user -> 
            defaultLayout [whamlet|<h1>logged in as #{userEmail user}|]
