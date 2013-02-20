module Bannerstalkerd where 

import Import
import Prelude
import Database.Persist
import Database.Persist.GenericSql.Raw
import Database.Persist.Postgresql
import Control.Monad
import Control.Monad.Trans.Resource
import Data.Text (unpack)
import Data.Time
import Data.Maybe
import qualified Data.Map as Map
import qualified Data.Set as Set

import CourseList
import Model
import Settings
import Email

-- Daemon function.
bannerstalkerd :: Extra -> PersistConfig -> IO ()
bannerstalkerd extra dbConf = do
    let conn = withPostgresqlConn (pgConnStr dbConf)
    let semesters = map unpack $ extraSemesters extra
    runResourceT $ conn $ runSqlConn $ do 
        runMigration migrateAll
        mapM_ refreshCourseList semesters
        blebleble
    where
        subject = "0"

        -- Applies the program to a given semester.
        refreshCourseList semester = do
            -- Pull current data from database.
            sectionsList <- selectList [SectionSemester ==. semester] []
            let keys = map (sectionCrn . entityVal) sectionsList
                oldSections = Map.fromList $ zip keys sectionsList
            -- Fetch new data from CourseList.
            response <- liftIO $ fetchCourseList semester subject
            time <- liftIO $ getCurrentTime
            case response of
                -- Server error: record statuses as Unavailable.
                Left err -> do
                    let recordUnavailable sectionId =
                            insert $ History sectionId time Unavailable
                        sectionIds = map entityKey $ Map.elems oldSections
                    mapM_ recordUnavailable sectionIds
                -- Process the new data.
                Right sectionsList -> do
                    let keys = map sectionCrn sectionsList
                        sections = Map.fromList $ zip keys sectionsList
                    processCourseListData time oldSections sections

        -- Updates Section and History with the new CourseList data.
        processCourseListData time oldSections newSections = do
            let -- Add new sections to database.
                handleAddedCrn crn = do
                    let section = fromJust $ Map.lookup crn newSections
                    sectionId <- insert section
                    insert $ History sectionId time $
                        sectionCurrStatus section
                    liftIO $ putStrLn $ "new class: " ++ show section
                -- TODO what to do when a class is removed?
                handleRemovedCrn crn = do
                    liftIO $ putStrLn "Class removed, what do I do?"
                    --let sectionId = entityKey $ fromJust $
                    --                    Map.lookup crn oldSections
                    --insert $ History sectionId time Unavailable
                -- Update changed statuses.
                handleExistingCrn crn = do
                    let (Entity sectionId
                                (Section _ _ _ _ _ _ _ _ oldStatus)) =
                            fromJust $ Map.lookup crn oldSections
                        (Section _ _ _ _ _ _ _ _ newStatus) =
                            fromJust $ Map.lookup crn newSections
                        -- XXX Remember to change back to /=
                    when (newStatus == oldStatus) $ do
                        scheduleNotification sectionId newStatus
                        update sectionId [SectionCurrStatus =. newStatus]
                    insert $ History sectionId time newStatus
                -- Partition sections into added, removed, and existing.
                oldCrns = Set.fromList $ Map.keys oldSections
                newCrns = Set.fromList $ Map.keys newSections
                addedCrns = Set.difference newCrns oldCrns 
                removedCrns = Set.difference oldCrns newCrns
                existingCrns = Set.intersection oldCrns newCrns
            -- Process the partitions.
            liftIO $ putStrLn $
                "added:    " ++ show (Set.size addedCrns) ++ "\n" ++
                "removed:  " ++ show (Set.size removedCrns) ++ "\n" ++
                "existing: " ++ show (Set.size existingCrns)
            mapM_ handleAddedCrn $ Set.toList addedCrns
            mapM_ handleRemovedCrn $ Set.toList removedCrns
            mapM_ handleExistingCrn $ Set.toList existingCrns

        -- Schedules notifications for the given section and its status.
        scheduleNotification sectionId currStatus = do
            requests <- selectList
                [SectionRequestSectionId ==. sectionId] []
            mapM_ (updateNotifications currStatus)
                (requests :: [Entity SectionRequest])
            
        -- Adds or removes notifications to keep them up to date.
        updateNotifications currStatus (Entity reqId req) = do
            -- The last status the user was notified of.
            let lastStatus = sectionRequestLastStatus req
            -- XXX Don't forget to change back to ==
            case (lastStatus /= currStatus) of
                -- Status is unchanged, delete notification.
                True -> do
                    deleteBy $ UniqueReqId reqId
                -- Insert a new notification.
                False -> do
                    -- XXX Remove when I change back all the /='s
                    deleteWhere [NotificationRequestId ==. reqId]
                    -- TODO time calculations go here
                    time <- liftIO getCurrentTime
                    insert $ Notification reqId time
                    return ()

        blebleble = do
            time <- liftIO $ getCurrentTime
            reqIds <- fmap (map $ notificationRequestId . entityVal) $
                                selectList [NotificationTime <. time] []
            requests <- selectList [SectionRequestId <-. reqIds] []
            users    <- selectList [] []
            sections <- selectList [] []
            let toMap entities = Map.fromList
                    [(entityKey e, entityVal e) | e <- entities]
                sectionMap = toMap (sections :: [Entity Section])
                userMap = toMap (users :: [Entity User])
                joinAndNotify (Entity reqId
                        (SectionRequest sectId userId lastStatus)) = do
                    let section = fromJust $ Map.lookup sectId sectionMap
                        user = fromJust $ Map.lookup userId userMap
                        currStatus = sectionCurrStatus section
                    -- Send the notification.
                    sendNotification user section
                    -- Update lastStatus
                    update reqId [SectionRequestLastStatus =. currStatus]
                    -- Delete the notification.
                    deleteBy $ UniqueReqId reqId
            mapM_ joinAndNotify requests

        sendNotification user section = do
            when (userUseEmail user) $ do
                liftIO $ putStrLn $
                    "notifying " ++ show (userEmail user) ++
                    " about " ++ show (sectionTitle section)
            when (userUseSms user) $ do
                liftIO $ putStrLn $
                    "notifying " ++ show (userPhoneNum user) ++
                    " about " ++ show (sectionTitle section)
                

{-
        -- Notify all users subscribed to this class that it has changed.
        notifyStatusChange sectionId newSection = do
            requestsList <- selectList
                [SectionRequestSectionId ==. sectionId] []
            when (not $ null requestsList) $ do
                let unwrapUserId = sectionRequestUserId . entityVal
                    userIds = map unwrapUserId requestsList
                mapM_ (notifyIndividual newSection)  userIds

        -- Notifies an individual user that their class has changed.
        notifyIndividual section userId = do
            user <- fmap (entityVal . head) $
                        selectList [UserId ==. userId] []
            when (userUseEmail user) $ do
                let email = userEmail user
                liftIO $ putStrLn $ "notifying " ++ show email
                mesg <- liftIO $ createEmail email section
                -- TODO calculate actual time to send
                time <- liftIO $ getCurrentTime
                --insert $ Notification EmailNotification mesg time
                return ()
            when (userUseSms user) $ do
                liftIO $ putStrLn "sending sms"
-}
