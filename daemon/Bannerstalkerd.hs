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

bannerstalkerd :: Extra -> PersistConfig -> IO ()
bannerstalkerd extra dbConf = do
    let conn = withPostgresqlConn (pgConnStr dbConf)
    let semesters = map unpack $ extraSemesters extra
    runResourceT $ conn $ runSqlConn $ do 
        runMigration migrateAll
        mapM_ handleSemester semesters
    where
        subject = "0"
        handleSemester semester = do
            sectionsList <- selectList [SectionSemester ==. semester] []
            let keys = map (sectionCrn . entityVal) sectionsList
                oldSections = Map.fromList $ zip keys sectionsList
            response <- liftIO $ fetchCourseList semester subject
            time <- liftIO $ getCurrentTime
            case response of
                Left err -> do
                    --updateWhere [SectionSemester ==. semester]
                    --            [SectionLastStatus =. Unavailable

                    -- Record statuses as Unavailable.
                    let recordUnavailable = recordHistory time Unavailable
                        sectionIds = map entityKey $ Map.elems oldSections
                    mapM_ recordUnavailable sectionIds
                Right sectionsList -> do
                    let keys = map sectionCrn sectionsList
                        sections = Map.fromList $ zip keys sectionsList
                    processCourseList time oldSections sections
        processCourseList time oldSections newSections = do
            let oldCrns = Set.fromList $ Map.keys oldSections
                newCrns = Set.fromList $ Map.keys newSections
                addedCrns = Set.difference newCrns oldCrns 
                removedCrns = Set.difference oldCrns newCrns
                existingCrns = Set.intersection oldCrns newCrns
                handleAddedCrn crn = do
                    let section = fromJust $ Map.lookup crn newSections
                        status = sectionLastStatus section
                    sectionId <- insert section
                    recordHistory time status sectionId
                handleRemovedCrn crn = do
                    liftIO $ putStrLn "OMG WHERE DID IT GO"
                handleExistingCrn crn = do
                    let entity = fromJust $ Map.lookup crn oldSections
                        sectionId = entityKey entity
                        oldSection = entityVal entity
                        newSection = fromJust $ Map.lookup crn newSections
                        oldStatus = sectionLastStatus oldSection
                        newStatus = sectionLastStatus newSection
                    -- XXX don't forget to change this back to /=
                    when (newStatus == oldStatus) $ do
                        update sectionId [SectionLastStatus =. newStatus]
                        notifyStatusChange sectionId newSection
                    recordHistory time newStatus sectionId
            liftIO $ putStrLn $
                "added: " ++ show (Set.size addedCrns) ++
                "\nremoved: " ++ show (Set.size removedCrns) ++
                "\nexisting: " ++ show (Set.size existingCrns)
            mapM_ handleAddedCrn $ Set.toList addedCrns
            mapM_ handleRemovedCrn $ Set.toList removedCrns
            mapM_ handleExistingCrn $ Set.toList existingCrns
        notifyStatusChange sectionId newSection = do
            -- Joins in Persist are next to unsupported, so this
            -- horseshit will have to do.
            -- TODO figure out how to do proper joins or raw sql
            --      to speed this up
            requestsList <- selectList
                [SectionRequestSectionId ==. sectionId] []
            when (not $ null requestsList) $ do
                let selectSettings userId =
                        selectList [SettingsUserId ==. userId] []
                    unwrapUserId = sectionRequestUserId . entityVal
                    userIds = map unwrapUserId requestsList 
                    unwrapSettingsList = map (entityVal . head)
                settingsList <- mapM selectSettings userIds
                let unwrappedSettings = unwrapSettingsList settingsList
                mapM_ (notifyIndividual newSection) unwrappedSettings
        notifyIndividual section settings = do
            when (settingsUseEmail settings) $ do
                let userId = settingsUserId settings
                emails <- selectList [EmailUserId ==. userId] []
                let email = emailEmail $ entityVal $ head emails
                liftIO $ putStrLn $ "notifying " ++ show email
                mesg <- liftIO $ createEmail email section
                -- TODO calculate actual time to send
                time <- liftIO $ getCurrentTime
                let n = Notification EmailNotification mesg time
                insert n
                return ()
            when (settingsUseSms settings) $ do
                liftIO $ putStrLn "sending sms"
        recordHistory time status sectionId = do
            insert $ History sectionId time status
            
    -- loop
        -- keys
        -- for each getCourseList
            -- add this key to keys
            -- if crn not in sections: add class
            -- else:
                -- prev = sections[section.crn]
                -- if section != sections[key]:
                    -- update database with new values?
            -- if key in statuses and old status != new status
                -- notify users

{-
startDaemon :: Settings.PersistConfig -> IO ()
--startDaemon = serviced daemon
startDaemon conf = do
    --putStrLn $ show $ pgConnStr conf
    courseList <- fetchCourseList "201320" "0"
    case courseList of
        Left err -> do
            putStrLn $  "Error fetching courselist: " ++ err
        Right sections -> do
            let firstSection = head sections
                conn = withPostgresqlConn (pgConnStr conf)
            runResourceT $ conn $ runSqlConn $ do
                runMigration migrateAll
                deleteWhere [SectionSemester ==. "201320"]
                sectionIds <- mapM insert sections    
                liftIO $ putStrLn $ show sectionIds 
-}
