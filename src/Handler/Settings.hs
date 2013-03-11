module Handler.Settings where

import Import
import Data.Maybe

data PasswordCreds = PasswordCreds Text Text

changePasswordForm :: FormInput App App PasswordCreds
changePasswordForm = PasswordCreds
    <$> ireq passwordField "password"
    <*> ireq passwordField "confirm"

getSettingsR :: Handler RepHtml
getSettingsR = do
    Entity userId user <- fmap fromJust currentUser
    semesterEntities <- runDB $
        selectList [SemesterActive ==. True] [Desc SemesterCode]
    let semesterIds = map entityKey semesterEntities
        semesters = map entityVal semesterEntities
    privileges <- fmap (map entityVal) $ runDB $ 
        selectList [ PrivilegeUserId ==. userId
                   , PrivilegeSemester <-. semesterIds ] []
    let privMap = [ (semesterCode sem, privilegeLevel priv)
                  | Entity semId sem <- semesterEntities
                  , priv <- privileges
                  , semId == privilegeSemester priv ]
    --let canSms = any (\p -> privilegeLevel p > Level1) privileges
    userSettings <- fmap (entityVal . fromJust) $
        runDB $ getBy $ UniqueUserSettings userId
    token <- getToken
    defaultLayout $ do
        setTitle "Settings"
        $(widgetFile "settings")

postSettingsR :: Handler RepHtml
postSettingsR = defaultLayout [whamlet|<h1>not implemented|]
