module Handler.Verify where

import Import
import Control.Monad
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import Data.Text.Lazy.Builder
import Network.Mail.Mime
import System.Random (newStdGen)
import Text.Blaze.Html.Renderer.String
import Text.Hamlet
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
                insert $ EmailVerification userId verKey
                return ()
            -- Update a previous record with new verKey.
            Just (Entity id _) -> do
                update id [EmailVerificationVerKey =. verKey]
                return ()
    -- Generate verUrl and create and send email.
    render <- getUrlRender
    tm <- getRouteToMaster
    let verUrl = render $ tm $ VerifyR userId verKey
        to = Address Nothing email
        from = noreplyAddr
        subject = "Bannerstalker verification email"
        text = LT.pack $ renderHtml
                $(shamletFile "templates/verification/mail-text.hamlet")
        html = LT.pack $ renderHtml
                $(shamletFile "templates/verification/mail-html.hamlet")
    liftIO $ simpleMail to from subject text html [] >>= mySendmail

getVerificationSentR :: Handler RepHtml
getVerificationSentR =
    defaultLayout [whamlet|<h1>verification sent|]

getVerifyR :: UserId -> Text -> Handler RepHtml
getVerifyR userId verKey = do
    currUserId <- currentUserId
    -- Skip if logged in.
    when (isNothing currUserId) $ do
        -- Ignore nonexistant users.
        mUser <- runDB $ get userId
        when (isNothing mUser) $ return ()
        mEmailVer <- runDB $ getBy $ UniqueUserEmailVer userId
        case mEmailVer of
            -- Doesn't have verification record, so ignore.
            Nothing -> return ()
            Just (Entity evKey (EmailVerification _ correctVerKey)) ->
                -- Verify and login if keys match.
                when (verKey == correctVerKey) $ do
                    runDB $ do
                        update userId [UserVerified =. True]
                        delete evKey
                    doLogin userId
    -- Always redirect to HomeR.
    redirectUltDest HomeR
