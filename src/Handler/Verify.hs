module Handler.Verify where

import Import
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import Data.Text.Lazy.Builder
import Network.Mail.Mime
import System.Random (newStdGen)
import Text.Blaze.Html.Renderer.String
import Text.Hamlet
import Text.Shakespeare.Text (textFile)
import Email

getResendVerificationR :: Handler RepHtml
getResendVerificationR =
    defaultLayout [whamlet|<h1>not implemented|]

postResendVerificationR :: Handler RepHtml
postResendVerificationR =
    defaultLayout [whamlet|<h1>not implemented|]

-- Assumes email maps to a valid user.
sendVerificationEmail :: Text -> Handler ()
sendVerificationEmail email = do
    Entity userId _ <- fmap fromJust $ runDB $ getBy $ UniqueEmail email
    -- Generate new verKey 
    stdgen <- liftIO newStdGen
    let verKey = T.pack $ fst $ randomString 20 stdgen
    runDB $ do
        mEmailVer <- getBy $ UniqueUserEmailVer userId 
        case mEmailVer of
            -- Insert new verification record.
            Nothing -> do
                _ <- insert $ EmailVerification userId verKey
                return ()
            -- Update a previous record with new verKey.
            Just (Entity evId  _) -> do
                update evId [EmailVerificationVerKey =. verKey]
                return ()
    -- Generate verUrl and create and send email.
    render <- getUrlRender
    tm <- getRouteToMaster
    let verUrl = render $ tm $ VerifyR userId verKey
        to = Address Nothing email
        from = noreplyAddr
        subject = "Bannerstalker verification email"
        text = toLazyText $ $(textFile "templates/verification.text") ()
        html = LT.pack $ renderHtml
            $(shamletFile "templates/verification.hamlet")
    liftIO $ simpleMail to from subject text html [] >>= mySendmail

getVerifyR :: UserId -> Text -> Handler RepHtml
getVerifyR userId verKey = do
    mCurrUser <- currentUser
    success <- case mCurrUser of
        -- Skip if logged in.
        Just _ -> return False
        Nothing -> do
            mUser <- runDB $ getBy $ UniqueUserEmailVer userId
            case mUser of
                -- Skip nonexistant users.
                Nothing -> return False
                Just (Entity evId (EmailVerification _ correctVerKey)) ->
                    -- Keys don't match.
                    if (verKey /= correctVerKey) then return False
                    -- Verify and login if keys match.
                    else do
                        runDB $ do
                            update userId [UserVerified =. True]
                            delete evId
                        doLogin userId
                        return True
    if success
        then redirect HomeR
        else defaultLayout [whamlet|
$newline never
<h3>This link is expired
<p .lead>
    Request another one
    <a href=@{ResendVerificationR}>here.
|]
