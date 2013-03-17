{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Home where

import Import
import Data.Maybe
import qualified Data.Text as T

topInfoKey, topErrorKey, addErrorKey :: Text
topInfoKey = "_HomeR_topInfo"
topErrorKey = "_HomeR_topError"
addErrorKey = "_HomeR_addError"

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
            -- Get all the request sectionIds.
            sectionIds <- fmap (map $ sectionRequestSectionId . entityVal) $
                runDB $  selectList [SectionRequestUserId  ==. userId] []
            (semesters, semestersMap) <-
                listSectionsHelper sectionIds =<< removeCrnForm
            subjectWidget <- selectSubject
            token <- getToken
            mTopInfo <- consumeSession topInfoKey
            mTopError <- consumeSession topErrorKey
            mAddError <- consumeSession addErrorKey
            defaultLayout $ do
                setTitle "Bannerstalker"
                $(widgetFile "home-logged-in")

listSectionsHelper :: [SectionId]
                      -> (Int -> Widget)
                      -> Handler ([Semester], [(Text, Widget)])
listSectionsHelper sectionIds crnWidget = do
    -- Get the list of semesters.
    semesters <- runDB $
        selectList [SemesterActive ==. True] [Desc SemesterCode]
    -- Create a map of semester codes to widgets.
    semestersMap <- fmap catMaybes $
        mapM (getSemesterWidgetPair sectionIds) semesters
    return (map entityVal semesters, semestersMap)
    where
        getSemesterWidgetPair sectIds (Entity semesterId semester) = do
            mWidget <- getSemesterWidget sectIds semesterId
            return $ case mWidget of
                Nothing -> Nothing
                Just widget -> Just (semesterCode semester, widget)
        getSemesterWidget sectIds semesterId = do
            sections <- fmap (map entityVal) $ runDB $
                selectList [ SectionId <-. sectIds
                           , SectionSemester ==. semesterId ]
                           [ Asc SectionSubject
                           , Asc SectionCourseId ]
            return $ case sections of
                [] -> Nothing
                _-> Just $(widgetFile "iter-sections")

postHomeR :: Handler RepHtml
postHomeR = do
    mUser <- currentUser
    case mUser of
        Nothing -> redirect HomeR
        Just (Entity userId _) -> do
            (postType, crn) <- runInputPost $ (,)
                <$> ireq textField "method"
                <*> ireq intField "crn"
            case postType of
                "add" -> do
                    mErr <- addCrn userId crn
                    case mErr of
                        Just err -> setSession addErrorKey err
                        Nothing -> setSession topInfoKey $ T.concat
                            ["CRN ", T.pack $ show crn, " added."]
                "remove" -> do
                    removeCrn userId crn >>= setSessionWith topInfoKey
                _ -> setSession addErrorKey formError
            redirect HomeR

getSearchR :: Handler RepHtml
getSearchR = do
    subject <- runInputGet $ ireq textField "subject"
    sectionIds <- fmap (map entityKey) $ runDB $
        selectList [SectionSubject ==. subject] [Asc SectionCrn]
    (semesters, semestersMap) <-
        listSectionsHelper sectionIds =<< addCrnForm
    subjectWidget <- selectSubject
    token <- getToken
    defaultLayout $ do
        setTitle "Search"
        $(widgetFile "search")

addCrn :: UserId -> Int -> Handler (Maybe Text)
addCrn userId crn = runDB $ do
    let textCrn = T.concat ["CRN ", T.pack $ show crn, "."]
    mSection <- getBy $ UniqueCrn crn
    case mSection of
        Nothing -> return $ Just $ T.concat
            ["There are no classes with ", textCrn]
        Just (Entity sectionId section) -> do
            let semester = sectionSemester section
            numCrns <- countCrns semester
            crnLimit <- getCrnLimit semester
            if numCrns < crnLimit
                then do
                    result <- insertBy $
                        SectionRequest sectionId userId Unavailable
                    case result of
                        Left _ -> return $ Just $ T.concat
                             ["You are already stalking ", textCrn]
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

removeCrn :: UserId -> Int -> Handler (Maybe Text)
removeCrn userId crn = runDB $ do
    mSection <- getBy $ UniqueCrn crn
    case mSection of
        Nothing -> return Nothing
        Just (Entity sectionId _) -> do
            mSectionRequest <- getBy $ UniqueRequest sectionId userId
            case mSectionRequest of
                Nothing -> return Nothing
                Just (Entity reqId _) -> do
                    deleteWhere [NotificationRequestId ==. reqId]
                    delete reqId
                    return $ Just $ T.concat
                        ["CRN ", T.pack $ show crn, " removed."]

iterSections :: [Section] -> (Int -> Widget) -> Handler Widget
iterSections sections crnWidget = do
    return $(widgetFile "iter-sections")

addCrnForm :: Handler (Int -> Widget)
addCrnForm = do
    token <- getToken
    return (\crn -> [whamlet|
$newline never
<form method=post action=@{HomeR}>
    ^{token}
    <input type=hidden name=method value=add>
    <input type=hidden name=crn value=#{crn}>
    <button type=submit .btn .btn>
        <i .icon-plus-sign>
        \ Add CRN
|])

removeCrnForm :: Handler (Int -> Widget)
removeCrnForm = do
    token <- getToken
    return (\crn -> [whamlet|
$newline never
<form method=post action=@{HomeR}>
    ^{token}
    <input type=hidden name=method value=remove>
    <input type=hidden name=crn value=#{crn}>
    <button type=submit .btn .btn>
        <i .icon-remove>
        \ Remove CRN
|])

selectSubject :: Handler Widget
selectSubject = return $(widgetFile "select-subject")

getInvalidR :: Handler RepHtml
getInvalidR = notFound
