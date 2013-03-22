module Bannerstalkerd where 

import Prelude
import Control.Concurrent
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Logger (runNoLoggingT)
import Control.Monad.Trans.Resource
import Database.Persist
import Database.Persist.GenericSql
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
        nextTimeInterval time interval =
            (*) interval $ (truncate $ time / fromIntegral interval) + 1

-- Single iteration of the daemon.
bannerstalkerd :: Extra -> PersistConfig -> Manager -> IO ()
bannerstalkerd extra dbConf manager = do
    let conn = withPostgresqlConn (pgConnStr dbConf)
    runNoLoggingT $ runResourceT $ conn $ runSqlConn $ do 
        runMigration migrateAll
        -- Get all active semesters in the form [(id, code)].
        semesterEntities <- selectList [SemesterActive ==. True] []
        let semesters = [(sId, semesterCode sem) |
                                 Entity sId sem <- semesterEntities]
        mapM_ refreshCourseList semesters

    where
        -- Applies the program to a given semester.
        refreshCourseList (semesterId, semesterCode) = do
            -- Pull current data from database.
            sectionsList <- selectList [SectionSemester ==. semesterId] []
            let keys = map (sectionCrn . entityVal) sectionsList
                oldSections = Map.fromList $ zip keys sectionsList
            -- Fetch new data from CourseList.
            response <- liftIO $ fetchCourseList
                                    manager semesterId semesterCode
            t <- liftIO $ getCurrentTime
            case response of
                -- Server error: record statuses as Unavailable.
                Left err -> do
                    insert $ CourseListLog t semesterId
                        Failure (Just err) Nothing Nothing Nothing
                    let recordUnavailable crn =
                            insert $ HistoryLog t crn Unavailable
                        crns = map (sectionCrn . entityVal) $
                            Map.elems oldSections
                    mapM_ recordUnavailable crns
                -- Process the new data.
                Right sectionsList -> do
                    let keys = map sectionCrn sectionsList
                        sections = Map.fromList $ zip keys sectionsList
                    processCourseListData semesterId t oldSections sections

        -- Updates Section and HistoryLog with the new CourseList data.
        processCourseListData semester t oldSections newSections = do
            let -- Add new sections to database.
                handleAddedCrn crn = do
                    let section = fromJust $ Map.lookup crn newSections
                    sectionId <- insert section
                    insert $ HistoryLog t crn $ sectionCurrStatus section
                -- Attempt to remove the section if no one is stalking it,
                -- otherwise manual intervention is required.
                handleRemovedCrn crn = do
                    let Entity sectionId _ =
                            fromJust $ Map.lookup crn oldSections
                    requests <- selectList
                        [SectionRequestSectionId ==. sectionId] []
                    case requests of
                        [] -> do
                            deleteBy $ UniqueCrn crn
                            liftIO $ mailAlert $
                                pack $ "crn removed: " ++ show crn
                        _ -> liftIO $ mailAlert $ pack $
                            "conflicts while removing crn: " ++ show crn
                -- Update changed statuses.
                handleExistingCrn crn = do
                    let (Entity sectionId
                                (Section  _ _ _ _ _ _ _ oldStatus)) =
                            fromJust $ Map.lookup crn oldSections
                        (Section _ _ _ _  _ _ _ newStatus) =
                            fromJust $ Map.lookup crn newSections
                    when (newStatus /= oldStatus) $ do
                        update sectionId [SectionCurrStatus =. newStatus]
                        commit
                        sendAllNotifications semester sectionId
                    insert $ HistoryLog t crn newStatus
                -- Partition sections into added, removed, and existing.
                oldCrns = Set.fromList $ Map.keys oldSections
                newCrns = Set.fromList $ Map.keys newSections
                addedCrns = Set.difference newCrns oldCrns 
                removedCrns = Set.difference oldCrns newCrns
                existingCrns = Set.intersection oldCrns newCrns
            insert $ CourseListLog t semester Success Nothing
                (Just $ Set.size addedCrns)
                (Just $ Set.size removedCrns)
                (Just $ Set.size existingCrns)
            -- Process the partitions.
            mapM_ handleAddedCrn $ Set.toList addedCrns
            mapM_ handleRemovedCrn $ Set.toList removedCrns
            mapM_ handleExistingCrn $ Set.toList existingCrns

        -- Schedules notifications for the given section and its status.
        sendAllNotifications semester sectionId = do
            requests <- fmap (map entityVal) $ selectList
                [SectionRequestSectionId ==. sectionId] []
            mapM_ sendNotification requests
            
        sendNotification (SectionRequest
                userId email phoneNum phoneCall sectionId) = do
            section <- fmap fromJust $ get sectionId
            -- Send email.
            (status, err) <- liftIO $ notifyEmail email section
            time <- liftIO $ getCurrentTime
            insert $ NotificationLog time (sectionCrn section)
                EmailNotification (Just userId) email status err
            -- Send SMS.
            (status, err) <- liftIO $ notifySms
                manager extra phoneNum section
            time <- liftIO $ getCurrentTime
            insert $ NotificationLog time (sectionCrn section)
                SmsNotification (Just userId) phoneNum status err
            -- Send phone call.
            if phoneCall
                then return ()
                else return ()
            

mailAlert :: Text -> IO ()
mailAlert text = do
    message <- simpleMail addr addr subject lazyText lazyText []
    mySendmail message
    where
        addr = adminAddr
        subject = "Bannerstalker Alert"
        lazyText = fromChunks [text]
