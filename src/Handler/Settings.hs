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
        canSms = any (\p -> privilegeLevel p > Level1) privileges
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
    (postType, mRawPhoneNum, mPasswd, mConfirm) <- runInputPost $ comma4
        <$> ireq textField "method"
        <*> iopt textField "phoneNum"
        <*> iopt textField "password"
        <*> iopt textField "confirm"
    case postType of
        "phone" -> updatePhoneNum userId mRawPhoneNum
        "password" -> case (mPasswd, mConfirm, mPasswd == mConfirm) of
            (Just passwd, Just _, True) -> do
                if T.length passwd < 8
                    then do
                        setSession passwordErrorKey passwordTooShort
                        redirect SettingsR
                    else do
                        changePassword userId passwd
                        setSession passwordInfoKey
                            "Your password has been updated."
                        redirect SettingsR
            _ -> do
                setSession passwordErrorKey passwordMismatch
                redirect SettingsR
        _ -> invalidArgs []

updatePhoneNum :: UserId -> Maybe Text -> Handler RepHtml
updatePhoneNum userId mRawPhoneNum = do
    case fmap validatePhoneNum mRawPhoneNum of
        -- Validation failed.
        Just Nothing -> do
            setSession phoneErrorKey
                "Please enter a valid phone number."
            redirect SettingsR
        -- Valid phone number.
        Just (Just phoneNum) -> do
            -- note to self, actually check if they are able to sms
            -- and make UpdateR work with this to make sure useSms
            -- is correct
            runDB $ update userId [UserPhoneNum =. Just phoneNum]
            setSession phoneInfoKey "Peter still needs to verify you are able to send SMS notifications..."
            redirect SettingsR
        -- Phone number was removed.
        Nothing -> do
            runDB $ update userId [UserPhoneNum =. Nothing]
            setSession phoneInfoKey "Phone number removed"
            redirect SettingsR
    where
        validatePhoneNum phoneNum = case T.take 2 phoneNum of
            "+1" -> _validatePhoneNum $ T.drop 2 phoneNum
            _ -> _validatePhoneNum phoneNum
        _validatePhoneNum rawPhoneNum =
            let phoneNum = T.filter isDigit rawPhoneNum
            in case T.length phoneNum of
                10 -> Just $ T.concat ["+1", phoneNum]
                _ -> Nothing
