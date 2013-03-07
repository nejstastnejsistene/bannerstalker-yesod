{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Home where

import Import
import Data.Maybe
import Data.Text (unpack)

import Handler.Auth

crnForm :: Widget -> Text -> Text -> Handler (Int -> Widget)
crnForm token action message = return (\crn -> [whamlet|
<form method=post action=@{HomeR}>
    ^{token}
    <input type=hidden name=type value=#{action}>
    <input type=hidden name=crn value=#{crn}>
    <button type=submit .btn .btn-small>#{message}
|])

getHomeR :: Handler RepHtml
getHomeR = do
    mUser <- currentUser
    case mUser of
        Nothing -> do
            (loginWidget, lEnctype) <- generateFormPost loginForm
            (registerWidget, rEnctype) <- generateFormPost registerForm
            defaultLayout $ do
                setTitle "Bannerstalker"
                $(widgetFile "home")
        Just user -> homeHelper user Nothing

homeHelper :: Entity User -> Maybe AppMessage -> Handler RepHtml
homeHelper (Entity userId user) mErrorMessage = do
    sectIds <- fmap (map $ sectionRequestSectionId . entityVal) $ runDB $
        selectList [SectionRequestUserId  ==. userId] []
    sections <- fmap (map entityVal) $ runDB $  
        selectList [SectionId <-. sectIds] [Asc SectionCrn]
    token <- getToken
    removeCrnForm <- crnForm token "remove" "Remove"
    sectionsWidget <- iterSections sections removeCrnForm
    subjectWidget <- selectSubject
    defaultLayout $ do
        setTitle "Bannerstalker"
        $(widgetFile "home-logged-in")

postHomeR :: Handler RepHtml
postHomeR = do
    mUser <- currentUser
    case mUser of
        Nothing -> redirectUltDest HomeR
        Just user -> do
            (postType, mTextCrn) <- runInputPost $ (,)
                                <$> ireq textField "type"
                                <*> iopt textField "crn"
            mErrorMessage <- case mTextCrn of
                Nothing -> return $ Just MsgInvalidCrn
                Just textCrn -> do
                    let pairs = reads $ unpack textCrn :: [(Int, String)]
                    case pairs of
                        [(crn, "")] -> case postType of
                            "add" -> addCrn (entityKey user) crn
                            "remove" -> removeCrn (entityKey user) crn
                            _ -> return $ Just MsgFormError
                        _ -> return $ Just MsgInvalidCrn
            homeHelper user mErrorMessage

getSearchR :: Handler RepHtml
getSearchR = do
    subject <- runInputGet $ ireq textField "subject"
    sections <- fmap (map entityVal) $ runDB $
        selectList [SectionSubject ==. subject] [Asc SectionCrn]
    token <- getToken
    addCrnForm <- crnForm token "add" "Add CRN"
    sectionsWidget <- iterSections sections addCrnForm
    subjectWidget <- selectSubject
    defaultLayout $ do
        setTitle "Search"
        $(widgetFile "search")

addCrn :: UserId -> Int -> Handler (Maybe AppMessage)
addCrn userId crn = runDB $ do
    mSection <- getBy $ UniqueCrn crn
    case mSection of
        Nothing -> return $ Just MsgInvalidCrn
        Just (Entity sectionId _) -> do
            result <- insertBy $ SectionRequest sectionId userId Unavailable
            case result of
                Left _ -> return $ Just MsgAlreadyStalking
                Right _ -> return Nothing

removeCrn :: UserId -> Int -> Handler (Maybe AppMessage)
removeCrn userId crn = runDB $ do
    mSection <- getBy $ UniqueCrn crn
    case mSection of
        Nothing -> return Nothing
        Just (Entity sectionId _) -> do
            deleteBy $ UniqueRequest sectionId userId
            return Nothing

iterSections :: [Section] -> (Int -> Widget) -> Handler Widget
iterSections sections crnWidget = do
    semesters <- runDB $ selectList [] []
    token <- getToken
    let semestersMap =
            [(sId, name) | Entity sId (Semester _ name _) <- semesters]
    return $(widgetFile "iter-sections")

selectSubject :: Handler Widget
selectSubject = return $(widgetFile "select-subject")
