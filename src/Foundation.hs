{-# LANGUAGE RankNTypes #-}
module Foundation where

import Prelude
import Yesod
import Yesod.Static
import Yesod.Default.Config
import Yesod.Default.Util (addStaticContentExternal)
import Network.HTTP.Conduit (Manager)
import qualified Settings
import Settings.Development (development)
import qualified Database.Persist.Store
import Settings.StaticFiles
import Database.Persist.GenericSql
import Settings (widgetFile, Extra (..))
import Text.Jasmine (minifym)
import Web.ClientSession (getKey)
import Text.Hamlet
import System.Log.FastLogger (Logger)
import Data.Char (isDigit)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.ByteString.Base64 as B64
import Data.Text.Encoding
import Model

-- | The site argument for your application. This can be a good place to
-- keep settings and values requiring initialization before your application
-- starts running, such as database connections. Every handler will have
-- access to the data present here.
data App = App
    { settings :: AppConfig DefaultEnv Extra
    , getStatic :: Static -- ^ Settings for static file serving.
    , connPool :: Database.Persist.Store.PersistConfigPool Settings.PersistConfig -- ^ Database connection pool.
    , httpManager :: Manager
    , persistConfig :: Settings.PersistConfig
    , appLogger :: Logger
    }

-- Set up i18n messages. See the message folder.
mkMessage "App" "messages" "en"

-- This is where we define all of the routes in our application. For a full
-- explanation of the syntax, please see:
-- http://www.yesodweb.com/book/handler
--
-- This function does three things:
--
-- * Creates the route datatype AppRoute. Every valid URL in your
--   application can be represented as a value of this type.
-- * Creates the associated type:
--       type instance Route App = AppRoute
-- * Creates the value resourcesApp which contains information on the
--   resources declared below. This is used in Handler.hs by the call to
--   mkYesodDispatch
--
-- What this function does *not* do is create a YesodSite instance for
-- App. Creating that instance requires all of the handler functions
-- for our application to be in scope. However, the handler functions
-- usually require access to the AppRoute datatype. Therefore, we
-- split these actions into two functions and place them in separate files.
mkYesodData "App" $(parseRoutesFile "config/routes")

type Form x = Html -> MForm App App (FormResult x, Widget)

-- Please see the documentation for the Yesod typeclass. There are a number
-- of settings which can be configured by overriding methods here.
instance Yesod App where
    approot = ApprootMaster $ appRoot . settings

    -- Admin pages.
    isAuthorized AdminR _ = isAdmin
    isAuthorized AdminUsersR _ = isAdmin
    isAuthorized (AdminEditUserR _) _ = isAdmin
    isAuthorized AdminSemestersR _ = isAdmin

    -- Must be logged in.
    isAuthorized StartOrderR _ = isLoggedIn
    isAuthorized ChooseCrnsR _ = isLoggedIn
    isAuthorized ContactInfoR _ = isLoggedIn
    isAuthorized ReviewOrderR _ = isLoggedIn
    isAuthorized AccountR _ = isLoggedIn
    isAuthorized (ViewRequestR _) _ = isLoggedIn
    isAuthorized (RemoveRequestR _) _ = isLoggedIn

    -- Always accessable.
    isAuthorized _ _ = return Authorized

    errorHandler NotFound = fmap chooseRep $ defaultLayout $ do
        setTitle "Not Found"
        toWidget $(widgetFile "404")
    errorHandler err = defaultErrorHandler err


    -- Store session data on the client in encrypted cookies,
    -- default session idle timeout is 120 minutes
    makeSessionBackend _ = do
        key <- getKey "config/client_session_key.aes"
        let timeout = 365 * 24 * 60 * 60 -- 1 year
        (getCachedDate, _closeDateCache) <- clientSessionDateCacher timeout
        return . Just $ clientSessionBackend2 key getCachedDate

    defaultLayout widget = do
        master <- getYesod
        route' <- getCurrentRoute
        tm <- getRouteToMaster
        let route = case route' of 
                Nothing -> InvalidR
                Just route'' -> tm route''
            adminRoute = fst (splitAt 5 $ show route) == "Admin"
        mUser <- currentUser
        token <- getToken

        -- We break up the default layout into two components:
        -- default-layout is the contents of the body tag, and
        -- default-layout-wrapper is the entire page. Since the final
        -- value passed to hamletToRepHtml cannot be a widget, this allows
        -- you to use normal widget features in default-layout.

        pc <- widgetToPageContent $ do
            addStylesheet $ StaticR css_bootstrap_css
            addStylesheet $ StaticR css_font_awesome_min_css
            $(widgetFile "default-layout")
        hamletToRepHtml $(hamletFile "templates/default-layout-wrapper.hamlet")

    -- This is done to provide an optimization for serving static files from
    -- a separate domain. Please see the staticRoot setting in Settings.hs
    urlRenderOverride y (StaticR s) =
        Just $ uncurry (joinPath y (Settings.staticRoot $ settings y)) $ renderRoute s
    urlRenderOverride _ _ = Nothing

    -- The page to be redirected to when authentication is required.
    authRoute _ = Just LoginR

    -- This function creates static content files in the static folder
    -- and names them based on a hash of their content. This allows
    -- expiration dates to be set far in the future without worry of
    -- users receiving stale content.
    addStaticContent =
        addStaticContentExternal minifym genFileName Settings.staticDir (StaticR . flip StaticRoute [])
      where
        -- Generate a unique filename based on the content itself
        genFileName lbs
            | development = "autogen-" ++ base64md5 lbs
            | otherwise   = base64md5 lbs

    -- Place Javascript at bottom of the body tag so the rest of the page loads first
    jsLoader _ = BottomOfBody

    -- What messages should be logged. The following includes all messages when
    -- in development, and warnings and errors in production.
    shouldLog _ _source level =
        development || level == LevelWarn || level == LevelError

-- How to run database actions.
instance YesodPersist App where
    type YesodPersistBackend App = SqlPersist
    runDB f = do
        master <- getYesod
        Database.Persist.Store.runPool
            (persistConfig master)
            f
            (connPool master)

-- This instance is required to use forms. You can modify renderMessage to
-- achieve customized and internationalized form validation messages.
instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage

-- | Get the 'Extra' value, used to hold data from the settings.yml file.
getExtra :: Handler Extra
getExtra = fmap (appExtra . settings) getYesod

-- Note: previous versions of the scaffolding included a deliver function to
-- send emails. Unfortunately, there are too many different options for us to
-- give a reasonable default. Instead, the information is available on the
-- wiki:
--
-- https://github.com/yesodweb/yesod/wiki/Sending-email

isAdmin :: forall sub. GHandler sub App AuthResult
isAdmin = do
    mUser <- currentUser
    return $ case mUser of
        Nothing -> AuthenticationRequired
        Just (Entity _ user) ->
            if userAdmin user
                then Authorized
                else Unauthorized "These aren't the routes you're looking for."

isLoggedIn :: forall sub. GHandler sub App AuthResult
isLoggedIn = do
    mUser <- currentUser
    return $ case mUser of
        Nothing -> AuthenticationRequired
        Just _ -> Authorized

credsKey :: Text
credsKey = "_ID"

doLogin :: UserId -> GHandler sub App ()
doLogin userId = setSession credsKey $ toPathPiece userId

doLogout :: GHandler sub App ()
doLogout = deleteSession credsKey

currentUser :: GHandler sub App (Maybe (Entity User))
currentUser = do
    mUserId <- lookupSession credsKey
    case mUserId of
        -- Not logged in.
        Nothing -> return Nothing
        Just sUserId -> case fromPathPiece sUserId of
            -- Invalid userId.
            Nothing -> do
                doLogout
                return Nothing
            Just userId -> do
                mUser <- runDB $ get userId
                case mUser of
                    -- User does not exist.
                    Nothing -> do
                        doLogout
                        return Nothing
                    -- Return user.
                    Just user -> return $ Just $ Entity userId user

getToken :: forall sub. GHandler sub App Widget
getToken = do
    req <- getRequest
    return [whamlet|
$newline never
$maybe token <- reqToken req
    <input type=hidden name=_token value=#{token}>
|]

data MessageType = InfoMessage | SuccessMessage | ErrorMessage
    deriving Eq

showMessage :: MessageType -> Maybe Text -> Maybe Widget -> Widget
showMessage messageType mMessage mAdditional = [whamlet|
$newline never
$maybe message <- mMessage
    $case messageType
        $of InfoMessage
            <div .alert .alert-info>
                <i .icon-exclamation-sign>
                    \ #{message}
                    $maybe additional <- mAdditional
                        \ ^{additional}
        $of SuccessMessage
            <div .alert .alert-success>
                <i .icon-ok>
                    \ #{message}
                    $maybe additional <- mAdditional
                        \ ^{additional}
        $of ErrorMessage
            <div .alert .alert-error>
                <i .icon-exclamation-sign>
                    \ #{message}
                    $maybe additional <- mAdditional
                        \ ^{additional}
|]

getSessionWith :: Text -> Handler (Maybe Text)
getSessionWith key = do
    session <- getSession
    return $ fmap decodeUtf8 $ Map.lookup key session

consumeSession :: Text -> Handler (Maybe Text)
consumeSession key = do
    session <- getSession
    deleteSession key
    return $ fmap decodeUtf8 $ Map.lookup key session

setSessionWith :: Text -> Maybe Text -> Handler ()
setSessionWith key mValue = case mValue of
    Just value -> setSession key value
    Nothing -> deleteSession key

formError, passwordMismatch, passwordTooShort :: Text
formError = "Form error. Please try again."
passwordMismatch = "The passwords do not match."
passwordTooShort = "Passwords must be at least 8 characters long."

validatePhoneNum :: Text -> Maybe Text
validatePhoneNum phoneNum = case T.take 2 phoneNum of
    "+1" -> _validatePhoneNum $ T.drop 2 phoneNum
    _ -> _validatePhoneNum phoneNum
    where
        _validatePhoneNum rawPhoneNum =
            let phoneNum = T.filter isDigit rawPhoneNum
            in case T.length phoneNum of
                10 -> Just $ T.concat ["+1", phoneNum]
                _ -> Nothing
