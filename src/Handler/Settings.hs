module Handler.Settings where

import Import
import Data.Maybe

data PasswordCreds = PasswordCreds Text Text

changePasswordForm :: Form PasswordCreds
changePasswordForm = renderDivs $ PasswordCreds
    <$> areq passwordField "New password" Nothing
    <*> areq passwordField "Confirm new password" Nothing

getSettingsR :: Handler RepHtml
getSettingsR = do
    Entity userId user <- fmap fromJust currentUser
    semesterEntities <- runDB $
        selectList [SemesterActive ==. True] [Asc SemesterCode]
    let semesterIds = map entityKey semesterEntities
        semesters = map entityVal semesterEntities
    privileges <- fmap (map entityVal) $ runDB $ 
        selectList [ PrivilegeUserId ==. userId
                   , PrivilegeSemester <-. semesterIds ] []
    let privMap = [ (semesterCode sem, privilegeLevel priv)
                  | Entity semId sem <- semesterEntities
                  , priv <- privileges
                  , semId == privilegeSemester priv ]
    let canSms = any (\p -> privilegeLevel p > Level1) privileges
    userSettings <- fmap (entityVal . fromJust) $
        runDB $ getBy $ UniqueUserSettings userId
    (widget, enctype) <- generateFormPost changePasswordForm
    token <- getToken
    defaultLayout $ do
        setTitle "Settings"
        $(widgetFile "settings")

postSettingsR :: Handler RepHtml
postSettingsR = defaultLayout [whamlet|<h1>not implemented|]
