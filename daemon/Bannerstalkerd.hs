module Bannerstalkerd where 

import Prelude
import Control.Concurrent
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Logger (runNoLoggingT)
import Control.Monad.Trans.Resource
import Database.Persist
import Database.Persist.GenericSql.Raw
import Database.Persist.Postgresql
import Data.Text (Text, pack, unpack)
import Data.Text.Lazy (fromChunks)
import Data.Time
import Data.Time.Clock.POSIX
import Data.Maybe
import qualified Data.Map as Map
import qualified Data.Set as Set
import Network.HTTP.Conduit
import Network.Mail.Mime

import CourseList
import Email
import Model
import Notification
import Settings

sleepInterval = 300 -- 5 Minutes

bannerstalkerdLoop :: Extra -> PersistConfig -> Manager -> IO ()
bannerstalkerdLoop extra conf manager = do
    -- Put this in an error block for now so I am notified of errors.
    result <- try $ bannerstalkerd extra conf manager
    case result of
        Left ex -> mailAlert $ pack $ show (ex :: SomeException)
        Right _ -> return ()
    doSleep
    bannerstalkerdLoop extra conf manager
    where
        -- Sleep until the next 5 minute interval begins.
        doSleep = do
            time  <- getPOSIXTime
            let nextTime = nextTimeInterval time sleepInterval
            threadDelay $ round $ 1000000 * (fromIntegral nextTime - time)

-- Single iteration of the daemon.
bannerstalkerd :: Extra -> PersistConfig -> Manager -> IO ()
bannerstalkerd extra dbConf manager = do
    let conn = withPostgresqlConn (pgConnStr dbConf)
    runNoLoggingT $ runResourceT $ conn $ runSqlConn $ do 
        runMigration migrateAll
        flushNotifications
        mapM_ refreshCourseList $ extraSemesters extra
        flushNotifications

    where
        -- Applies the program to a given semester.
        refreshCourseList semester = do
            -- Pull current data from database.
            sectionsList <- selectList [SectionSemester ==. semester] []
            let keys = map (sectionCrn . entityVal) sectionsList
                oldSections = Map.fromList $ zip keys sectionsList
            -- Fetch new data from CourseList.
            response <- liftIO $ fetchCourseList manager semester
            t <- liftIO $ getCurrentTime
            case response of
                -- Server error: record statuses as Unavailable.
                Left err -> do
                    let recordUnavailable sectionId =
                            insert $ HistoryLog t sectionId Unavailable
                        sectionIds = map entityKey $ Map.elems oldSections
                    insert $ CourseListLog t Failure (Just err)
                                        Nothing Nothing Nothing
                    mapM_ recordUnavailable sectionIds
                -- Process the new data.
                Right sectionsList -> do
                    let keys = map sectionCrn sectionsList
                        sections = Map.fromList $ zip keys sectionsList
                    processCourseListData semester t oldSections sections

        -- Updates Section and HistoryLog with the new CourseList data.
        processCourseListData semester t oldSections newSections = do
            let -- Add new sections to database.
                handleAddedCrn crn = do
                    let section = fromJust $ Map.lookup crn newSections
                    sectionId <- insert section
                    insert $ HistoryLog t sectionId $
                        sectionCurrStatus section
                -- TODO what to do when a class is removed?
                -- For now I'll just handle these manually if it comes up.
                handleRemovedCrn crn = liftIO $
                    mailAlert $ pack $ "crn removed: " ++ show crn
                -- Update changed statuses.
                handleExistingCrn crn = do
                    let (Entity sectionId
                                (Section _ _ _ _ _ _ _ _ oldStatus)) =
                            fromJust $ Map.lookup crn oldSections
                        (Section _ _ _ _ _ _ _ _ newStatus) =
                            fromJust $ Map.lookup crn newSections
                    when (newStatus /= oldStatus) $ do
                        queueNotifications semester sectionId newStatus
                        update sectionId [SectionCurrStatus =. newStatus]
                    insert $ HistoryLog t sectionId newStatus
                -- Partition sections into added, removed, and existing.
                oldCrns = Set.fromList $ Map.keys oldSections
                newCrns = Set.fromList $ Map.keys newSections
                addedCrns = Set.difference newCrns oldCrns 
                removedCrns = Set.difference oldCrns newCrns
                existingCrns = Set.intersection oldCrns newCrns
            insert $ CourseListLog t Success Nothing
                (Just $ Set.size addedCrns)
                (Just $ Set.size removedCrns)
                (Just $ Set.size existingCrns)
            -- Process the partitions.
            mapM_ handleAddedCrn $ Set.toList addedCrns
            mapM_ handleRemovedCrn $ Set.toList removedCrns
            mapM_ handleExistingCrn $ Set.toList existingCrns

        -- Schedules notifications for the given section and its status.
        queueNotifications semester sectionId currStatus = do
            requests <- selectList
                [SectionRequestSectionId ==. sectionId] []
            mapM_ (updateNotifications semester currStatus) requests
            
        -- Adds or removes notifications to keep them up to date.
        updateNotifications semester currStatus (Entity reqId req) = do
            -- The last status the user was notified of.
            if sectionRequestLastStatus req == currStatus
                -- Status is unchanged, delete notification.
                then deleteBy $ UniqueReqId reqId
                -- Insert a new notification.
                else do
                    let userId = sectionRequestUserId req
                    user <- fmap fromJust $ get userId
                    priv <- fmap (entityVal . fromJust) $ getBy $
                        UniquePrivilege userId semester
                    sendTime <- liftIO $ nextNotificationTime $
                        getNotifyInterval extra $ privilegeLevel priv
                    insert $ Notification reqId sendTime
                    return ()

        -- Sends all notifications whose time has passed.
        flushNotifications = do
            time <- liftIO $ getCurrentTime
            reqIds <- fmap (map $ notificationRequestId . entityVal) $
                                selectList [NotificationTime <. time] []
            requests <- selectList [SectionRequestId <-. reqIds] []
            sections <- selectList [] []
            let sectionMap = Map.fromList [(k, v) | Entity k v <- sections]
                joinAndNotify (Entity reqId
                        (SectionRequest sectId userId lastStatus)) = do
                    let section = fromJust $ Map.lookup sectId sectionMap
                        currStatus = sectionCurrStatus section
                    -- Send the notification.
                    sendNotification userId section
                    -- Update lastStatus
                    update reqId [SectionRequestLastStatus =. currStatus]
                    -- Delete the notification.
                    deleteBy $ UniqueReqId reqId
            mapM_ joinAndNotify requests

        -- Check user settings and send mail and sms notifications
        -- for the given section.
        sendNotification userId section = do
            user <- fmap fromJust $ get userId
            settings <- fmap (entityVal . fromJust) $
                            getBy $ UniqueUserSettings (userId :: UserId)
            -- Email notifications.
            when (settingsUseEmail settings) $ do
                let email = userEmail user
                (status, err) <- liftIO $ notifyEmail email section
                time <- liftIO $ getCurrentTime
                insert $ NotificationLog time
                    EmailNotification email status err
                return ()
            -- Sms notifications.
            when (settingsUseSms settings) $ do
                let phoneNum = fromJust $ settingsPhoneNum settings
                (status, err) <- liftIO $
                    notifySms manager extra phoneNum section
                time <- liftIO $ getCurrentTime
                insert $ NotificationLog time 
                    SmsNotification phoneNum status err
                return ()

mailAlert :: Text -> IO ()
mailAlert text = do
    message <- simpleMail addr addr subject lazyText lazyText []
    mySendmail message
    where
        addr = adminAddr
        subject = "Bannerstalker Alert"
        lazyText = fromChunks [text]

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
