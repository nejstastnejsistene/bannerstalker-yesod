module Handler.Home where

import Import
import Data.Maybe
import Database.Persist.GenericSql

getHomeR :: Handler RepHtml
getHomeR = do
    defaultLayout $ do
        setTitle "Bannerstalker"
        $(widgetFile "home")

getAccountR :: Handler RepHtml
getAccountR = do
    let sql = "SELECT ??, ?? \
              \FROM \"user\", section, section_request \
              \WHERE section_request.user_id = ? \
                \AND section_request.section_id  = section.id \
              \ORDER BY section.course_id ASC"
    userId <- fmap (entityKey .fromJust) currentUser
    sqlResult <- runDB $ rawSql sql [toPersistValue userId]
    let requestSections = [(r, s) | (Entity _ r, Entity _ s) <- sqlResult]
    defaultLayout $ do
        setTitle "Account"
        $(widgetFile "account")

postAccountR :: Handler RepHtml
postAccountR = redirect AccountR

getInvalidR :: Handler RepHtml
getInvalidR = notFound
