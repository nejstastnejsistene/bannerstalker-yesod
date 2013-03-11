module Handler.Settings where

import Import
import Data.Char
import Data.Maybe
import qualified Data.Text as T

import Handler.Auth

phoneInfoKey, phoneErrorKey :: Text
phoneInfoKey = "_SettingsR_mPhoneInfo"
phoneErrorKey = "_SettingsR_mPhoneError"

passwordInfoKey, passwordErrorKey :: Text
passwordInfoKey = "_SettingsR_mPasswordInfo"
passwordErrorKey = "_SettingsR_mPasswordError"

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
    userSettings <- fmap (entityVal . fromJust) $
        runDB $ getBy $ UniqueUserSettings userId
    mPhoneInfo <- getSessionWith phoneInfoKey
    mPhoneError <- getSessionWith phoneErrorKey
    mPasswordInfo <- getSessionWith passwordInfoKey
    mPasswordError <- getSessionWith passwordErrorKey
    deleteSession phoneInfoKey
    deleteSession phoneErrorKey
    deleteSession passwordInfoKey
    deleteSession passwordErrorKey
    token <- getToken
    defaultLayout $ do
        setTitle "Settings"
        $(widgetFile "settings")

postSettingsR :: Handler RepHtml
postSettingsR = do
    Entity userId _ <- fmap fromJust currentUser
    let comma4 a b c d = (a, b, c, d)
    (postType, mPhoneNum, mPasswd, mConfirm) <- runInputPost $ comma4
        <$> ireq textField "method"
        <*> iopt textField "phoneNum"
        <*> iopt textField "password"
        <*> iopt textField "confirm"
    case postType of
        "phone" -> do
            let validated = fmap validatePhoneNum mPhoneNum
            case validated of
                -- validation failed
                Just Nothing -> do
                    setSession phoneErrorKey
                        "Please enter a valid phone number."
                    redirectUltDest SettingsR
                val -> do
                    let newVal = case val of
                            Nothing -> Nothing
                            Just (Just valid) -> Just valid
                    -- note to self, actually check if they are able to sms
                    -- and make UpdateR work with this to make sure useSms
                    -- is correct
                    settingsId <- fmap (entityKey . fromJust) $
                        runDB $ getBy $ UniqueUserSettings userId
                    runDB $ update settingsId [SettingsPhoneNum =. newVal]
                    setSession phoneInfoKey "Peer still needs to verify you are able to send SMS notifications..."
                    redirectUltDest SettingsR
        "password" -> case (mPasswd, mConfirm, mPasswd == mConfirm) of
            (Just passwd, Just _, True) -> do
                if T.length passwd < 8
                    then do
                        setSession passwordErrorKey passwordTooShort
                        redirectUltDest SettingsR
                    else do
                        changePassword userId passwd
                        setSession passwordInfoKey
                            "Your password has been updated."
                        redirectUltDest SettingsR
            _ -> do
                setSession passwordErrorKey passwordMismatch
                redirectUltDest SettingsR
        _ -> invalidArgs []
    where
        validatePhoneNum phoneNum = case T.take 2 phoneNum of
            "+1" -> _validatePhoneNum $ T.drop 2 phoneNum
            _ -> _validatePhoneNum phoneNum
        _validatePhoneNum rawPhoneNum =
            let phoneNum = T.filter isDigit rawPhoneNum
            in case T.length phoneNum of
                10 -> Just $ T.concat ["+1", phoneNum]
                _ -> Nothing
