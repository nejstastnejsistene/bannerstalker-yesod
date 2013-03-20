module Handler.Settings where

import Import
import Data.Char
--import Data.Maybe
import qualified Data.Text as T
import Database.Persist.GenericSql

--import Handler.Auth

phoneSuccessKey, phoneErrorKey :: Text
phoneSuccessKey = "_SettingsR_phoneSuccess"
phoneErrorKey = "_SettingsR_phoneError"

passwordSuccessKey, passwordErrorKey :: Text
passwordSuccessKey = "_SettingsR_passwordSuccess"
passwordErrorKey = "_SettingsR_passwordError"

getSettingsR :: Handler RepHtml
getSettingsR = do
    {-
    Entity userId user <- fmap fromJust currentUser
    let sql = "SELECT ??, ?? \
              \FROM semester, privilege \
              \WHERE semester.active = true \
                \AND semester.id = privilege.semester \
                \AND privilege.user_id = ? \
              \ORDER BY semester.code DESC"
    sqlResult <- runDB $ rawSql sql [toPersistValue userId]
    let semesterPrivs = [(s, p) | (Entity _ s, Entity _ p) <- sqlResult]
        canSms = any (\(_, p)-> privilegeLevel p > Level1) semesterPrivs
    mPhoneSuccess <- consumeSession phoneErrorKey
    mPhoneError <- consumeSession phoneErrorKey
    mPasswordSuccess <- consumeSession passwordSuccessKey
    mPasswordError <- consumeSession passwordErrorKey
    token <- getToken
    -}
    defaultLayout $ do
        setTitle "Settings"
        -- $(widgetFile "settings")
        [whamlet|ASDFASDF|]

postSettingsR :: Handler RepHtml
postSettingsR = do
    {-
    Entity userId _ <- fmap fromJust currentUser
    (postType, mRawPhoneNum, mPasswd, mConfirm) <- runInputPost $ (,,,)
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
    -}
    redirect SettingsR

updatePhoneNum :: UserId -> Maybe Text -> Handler RepHtml
updatePhoneNum userId mRawPhoneNum = do
    --
    --
    -- Check here if they canSms
    --
    --
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
