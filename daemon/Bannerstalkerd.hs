module Bannerstalkerd where 

import Import
import Prelude
import Control.Concurrent
import Control.Monad
import Control.Monad.Trans.Resource
import Database.Persist
import Database.Persist.GenericSql.Raw
import Database.Persist.Postgresql
import Data.Text (unpack)
import Data.Time
import Data.Time.Clock.POSIX
import Data.Maybe
import qualified Data.Map as Map
import qualified Data.Set as Set
import Network.HTTP.Conduit

import CourseList
import Model
import Notification
import Settings

bannerstalkerdLoop :: Extra -> PersistConfig -> Manager -> IO ()
bannerstalkerdLoop extra conf manager = do
        time <- getCurrentTime
        bannerstalkerd extra conf manager
        doSleep
        bannerstalkerdLoop extra conf manager
    where
        -- Sleep until the next 5 minute interval begins.
        doSleep = do
            time  <- getPOSIXTime
            let nextTime = fromIntegral $ nextTimeInterval time 300
            threadDelay $ round $ 1000000 * (nextTime - time)

-- Single iteration of the daemon.
bannerstalkerd :: Extra -> PersistConfig -> Manager -> IO ()
bannerstalkerd extra dbConf manager = do
    let conn = withPostgresqlConn (pgConnStr dbConf)
    let semesters = map unpack $ extraSemesters extra
    runResourceT $ conn $ runSqlConn $ do 
        runMigration migrateAll
        flushNotifications
        mapM_ refreshCourseList semesters
        flushNotifications

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
                            insert $ HistoryLog time sectionId Unavailable
                        sectionIds = map entityKey $ Map.elems oldSections
                    insert $ CourseListLog time Failure
                                    Nothing Nothing Nothing
                    mapM_ recordUnavailable sectionIds
                -- Process the new data.
                Right sectionsList -> do
                    let keys = map sectionCrn sectionsList
                        sections = Map.fromList $ zip keys sectionsList
                    processCourseListData time oldSections sections

        -- Updates Section and HistoryLog with the new CourseList data.
        processCourseListData time oldSections newSections = do
            let -- Add new sections to database.
                handleAddedCrn crn = do
                    let section = fromJust $ Map.lookup crn newSections
                    sectionId <- insert section
                    insert $ HistoryLog time sectionId $
                        sectionCurrStatus section
                    liftIO $ putStrLn $ "new class: " ++ show section
                -- TODO what to do when a class is removed?
                handleRemovedCrn crn = do
                    liftIO $ putStrLn "Class removed, what do I do?"
                    --let sectionId = entityKey $ fromJust $
                    --                    Map.lookup crn oldSections
                    --insert $ HistoryLog time sectionId Unavailable
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
                    insert $ HistoryLog time sectionId newStatus
                -- Partition sections into added, removed, and existing.
                oldCrns = Set.fromList $ Map.keys oldSections
                newCrns = Set.fromList $ Map.keys newSections
                addedCrns = Set.difference newCrns oldCrns 
                removedCrns = Set.difference oldCrns newCrns
                existingCrns = Set.intersection oldCrns newCrns
            insert $ CourseListLog time Success
                (Just $ Set.size addedCrns)
                (Just $ Set.size removedCrns)
                (Just $ Set.size existingCrns)
            liftIO $ putStrLn $
                "added:    " ++ show (Set.size addedCrns) ++ "\n" ++
                "removed:  " ++ show (Set.size removedCrns) ++ "\n" ++
                "existing: " ++ show (Set.size existingCrns)
            -- Process the partitions.
            mapM_ handleAddedCrn $ Set.toList addedCrns
            mapM_ handleRemovedCrn $ Set.toList removedCrns
            mapM_ handleExistingCrn $ Set.toList existingCrns

        -- Schedules notifications for the given section and its status.
        scheduleNotification sectionId currStatus = do
            requests <- selectList
                [SectionRequestSectionId ==. sectionId] []
            mapM_ (updateNotifications currStatus) requests
            
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
                    user <- fmap fromJust $ get $ sectionRequestUserId req
                    newTime <- liftIO $ nextNotificationTime $
                        getNotifyInterval extra $ userPrivilege user
                    insert $ Notification reqId newTime
                    return ()

        -- Sends all notifications whose time has passed.
        flushNotifications = do
            time <- liftIO $ getCurrentTime
            reqIds <- fmap (map $ notificationRequestId . entityVal) $
                                selectList [NotificationTime <. time] []
            requests <- selectList [SectionRequestId <-. reqIds] []
            users    <- selectList [] []
            sections <- selectList [] []
            let toMap entities = Map.fromList
                    [(entityKey e, entityVal e) | e <- entities]
                sectionMap = toMap sections
                userMap = toMap users
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

        -- Check user settings and send mail and sms notifications
        -- for the given section.
        sendNotification user section = do
            when (userUseEmail user) $ do
                let email = userEmail user
                (status, err) <- liftIO $ notifyEmail email section
                time <- liftIO $ getCurrentTime
                insert $ NotificationLog time
                    EmailNotification email status err
                return ()
            when (userUseSms user) $ do
                let phoneNum = userPhoneNum user
                (status, err) <- liftIO $
                    notifySms manager extra phoneNum section
                time <- liftIO $ getCurrentTime
                insert $ NotificationLog time 
                    SmsNotification phoneNum status err
                return ()
                
-- Takes a posix time and returns the start time in seconds of the
-- beginning of the next time interval.
nextTimeInterval :: POSIXTime -> Int -> Int
nextTimeInterval time interval =
    (*) interval $ (truncate $ time / fromIntegral interval) + 1

-- Determines the next time that a notification should be sent.
-- For example, if interval is two hours, this will return the time
-- rounded up to the next time with an hour divisible by 2. A negative
-- interval will do the same thing except round down instead which
-- is useful for 'ASAP' notifications.
nextNotificationTime :: Int -> IO UTCTime
nextNotificationTime interval = do
    utcTime  <- liftIO $ getPOSIXTime
    timeZone <- liftIO $ getCurrentTimeZone
    let tzSeconds = 60 * timeZoneMinutes timeZone
        localTime = utcTime + fromIntegral tzSeconds
        nextTime = (nextTimeInterval localTime interval) - tzSeconds
    return $ posixSecondsToUTCTime $ fromIntegral nextTime
