{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Home where

import Import
import Data.Maybe
import Data.Text (unpack)

import Handler.Auth

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
        selectList [SectionId <-. sectIds] [Asc SectionId]
    semesters <- runDB $ selectList [SemesterActive ==. True] []
    let semestersMap = [ (sId, name)
                       | Entity sId (Semester _ name _) <- semesters ]
    token <- getToken
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
