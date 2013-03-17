module Handler.Settings where

import Import
import Data.Char
import Data.Maybe
import qualified Data.Text as T

import Handler.Auth

phoneSuccessKey, phoneErrorKey :: Text
phoneSuccessKey = "_SettingsR_phoneSuccess"
phoneErrorKey = "_SettingsR_phoneError"

passwordSuccessKey, passwordErrorKey :: Text
passwordSuccessKey = "_SettingsR_passwordSuccess"
passwordErrorKey = "_SettingsR_passwordError"

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
    mPhoneSuccess <- consumeSession phoneErrorKey
    mPhoneError <- consumeSession phoneErrorKey
    mPasswordSuccess <- consumeSession passwordSuccessKey
    mPasswordError <- consumeSession passwordErrorKey
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
                        setSession passwordSuccessKey
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
            runDB $ update userId [UserPhoneNum =. Just phoneNum]
            setSession phoneSuccessKey "Phone number set"
            redirect SettingsR
        -- Phone number was removed.
        Nothing -> do
            runDB $ update userId [UserPhoneNum =. Nothing]
            setSession phoneSuccessKey "Phone number removed"
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
