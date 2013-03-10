{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Home where

import Import
import Data.Maybe

mainErrorKey, addErrorKey :: Text
mainErrorKey = "_HomeR_mMainError"
addErrorKey = "_HomeR_mAddError"

getHomeR :: Handler RepHtml
getHomeR = do
    mUser <- currentUser
    case mUser of
        Nothing -> do
            token <- getToken
            defaultLayout $ do
                setTitle "Bannerstalker"
                $(widgetFile "home")
        Just (Entity userId user) -> do
            sectIds <- fmap (map $ sectionRequestSectionId . entityVal) $
                runDB $  selectList [SectionRequestUserId  ==. userId] []
            sections <- fmap (map entityVal) $ runDB $  
                selectList [SectionId <-. sectIds] [Asc SectionCrn]
            sectionsWidget <- iterSections sections =<< removeCrnForm
            subjectWidget <- selectSubject
            token <- getToken
            -- Retrieve error messages from session.
            mMainError <- getSessionWith mainErrorKey
            mAddError <- getSessionWith addErrorKey
            -- Reset error messages.
            deleteSession mainErrorKey
            deleteSession addErrorKey
            defaultLayout $ do
                setTitle "Bannerstalker"
                $(widgetFile "home-logged-in")

postHomeR :: Handler RepHtml
postHomeR = do
    mUser <- currentUser
    case mUser of
        Nothing -> redirectUltDest HomeR
        Just (Entity userId _) -> do
            (postType, crn) <- runInputPost $ (,)
                <$> ireq textField "method"
                <*> ireq intField "crn"
            case postType of
                "add" -> addCrn userId crn >>= setSessionWith addErrorKey
                "remove" -> removeCrn userId crn
                _ -> setSession addErrorKey formError
            redirectUltDest HomeR

getSearchR :: Handler RepHtml
getSearchR = do
    subject <- runInputGet $ ireq textField "subject"
    sections <- fmap (map entityVal) $ runDB $
        selectList [SectionSubject ==. subject] [Asc SectionCrn]
    sectionsWidget <- iterSections sections =<< addCrnForm
    subjectWidget <- selectSubject
    token <- getToken
    defaultLayout $ do
        setTitle "Search"
        $(widgetFile "search")

addCrn :: UserId -> Int -> Handler (Maybe Text)
addCrn userId crn = runDB $ do
    mSection <- getBy $ UniqueCrn crn
    case mSection of
        Nothing -> return $ Just "Invalid Crn"
        Just (Entity sectionId section) -> do
            let semester = sectionSemester section
            numCrns <- countCrns semester
            crnLimit <- getCrnLimit semester
            if numCrns < crnLimit
                then do
                    result <- insertBy $
                        SectionRequest sectionId userId Unavailable
                    case result of
                        Left _ -> return $ Just "Already stalking"
                        Right _ -> return Nothing
                else return $ Just "Crn limit reached"
    where
        countCrns semester = do
            sections <- fmap (map entityVal) $
                selectList [SectionRequestUserId ==. userId] []
            count
                 [ SectionId <-. (map sectionRequestSectionId sections)
                 , SectionSemester ==. semester ]
        getCrnLimit semester = do
            mPrivilege <- getBy $ UniquePrivilege userId semester
            level <- case mPrivilege of
                Nothing -> do 
                    _ <- insert $ Privilege userId semester Level1
                    return Level1
                Just (Entity _ privilege) ->
                    return $ privilegeLevel privilege
            return $ case level of
                    Level1 -> 1
                    Level2 -> 5
                    Level3 -> 10
                    Admin -> 100


removeCrn :: UserId -> Int -> Handler ()
removeCrn userId crn = runDB $ do
    mSection <- getBy $ UniqueCrn crn
    case mSection of
        Nothing -> return ()
        Just (Entity sectionId _) ->
            deleteBy $ UniqueRequest sectionId userId

iterSections :: [Section] -> (Int -> Widget) -> Handler Widget
iterSections sections crnWidget = do
    semesters <- runDB $ selectList [] []
    let semestersMap =
            [(sId, name) | Entity sId (Semester _ name _) <- semesters]
    return $(widgetFile "iter-sections")

addCrnForm :: Handler (Int -> Widget)
addCrnForm = do
    token <- getToken
    return (\crn -> [whamlet|
<form method=post action=@{HomeR}>
    ^{token}
    <input type=hidden name=method value=add>
    <input type=hidden name=crn value=#{crn}>
    <button type=submit .btn .btn-small>Add CRN
|])

removeCrnForm :: Handler (Int -> Widget)
removeCrnForm = do
    token <- getToken
    return (\crn -> [whamlet|
<form method=post action=@{HomeR}>
    ^{token}
    <input type=hidden name=method value=remove>
    <input type=hidden name=crn value=#{crn}>
    <button type=submit .btn .btn-small>Remove CRN
|])

selectSubject :: Handler Widget
selectSubject = return $(widgetFile "select-subject")

getInvalidR :: Handler RepHtml
getInvalidR = notFound
