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
        mapM_ handleSemester semesters
    where
        subject = "0"

        -- Applies the program to a given semester.
        handleSemester semester = do
            sectionsList <- selectList [SectionSemester ==. semester] []
            let keys = map (sectionCrn . entityVal) sectionsList
                oldSections = Map.fromList $ zip keys sectionsList
            response <- liftIO $ fetchCourseList semester subject
            time <- liftIO $ getCurrentTime
            case response of
                -- Server error: record statuses as Unavailable.
                Left err -> do
                    let recordUnavailable sectionId =
                            insert $ History sectionId time Unavailable
                        sectionIds = map entityKey $ Map.elems oldSections
                    mapM_ recordUnavailable sectionIds
                -- Go to main logic.
                Right sectionsList -> do
                    let keys = map sectionCrn sectionsList
                        sections = Map.fromList $ zip keys sectionsList
                    processCourseList time oldSections sections

        -- Main logic function.
        processCourseList time oldSections newSections = do
            let oldCrns = Set.fromList $ Map.keys oldSections
                newCrns = Set.fromList $ Map.keys newSections
                addedCrns = Set.difference newCrns oldCrns 
                removedCrns = Set.difference oldCrns newCrns
                existingCrns = Set.intersection oldCrns newCrns

                -- Simply add new sections to database.
                handleAddedCrn crn = do
                    let section = fromJust $ Map.lookup crn newSections
                        status = sectionLastStatus section
                    sectionId <- insert section
                    insert $ History sectionId time status

                -- TODO what to do when a class is removed?
                handleRemovedCrn crn = do
                    liftIO $ putStrLn "OMG WHERE DID IT GO"

                -- Compare new and old statuses, and notify users
                -- accordingly.
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
                    insert $ History sectionId time newStatus
            liftIO $ putStrLn $
                "added: " ++ show (Set.size addedCrns) ++
                "\nremoved: " ++ show (Set.size removedCrns) ++
                "\nexisting: " ++ show (Set.size existingCrns)
            mapM_ handleAddedCrn $ Set.toList addedCrns
            mapM_ handleRemovedCrn $ Set.toList removedCrns
            mapM_ handleExistingCrn $ Set.toList existingCrns

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
                insert $ Notification EmailNotification mesg time
                return ()
            when (userUseSms user) $ do
                liftIO $ putStrLn "sending sms"
