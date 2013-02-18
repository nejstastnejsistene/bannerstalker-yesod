module Bannerstalkerd where 
import Import
import Prelude
import Database.Persist
import Database.Persist.Postgresql
import Control.Monad.Trans.Resource
import Data.Time
import qualified Data.Map as Map

import CourseList
import Model
import Settings (PersistConfig)

bannerstalkerd :: PersistConfig -> IO ()
bannerstalkerd dbConf = do
    let conn = withPostgresqlConn (pgConnStr dbConf)
    runResourceT $ conn $ runSqlConn $ do 
        sectionsList <- selectList [SectionSemester ==. semester] []
        let sectionKeys = map (sectionCrn . entityVal) sectionsList
            sections = Map.fromList $ zip sectionKeys sectionsList
        checkCourseList sections
    return ()
    where
        semester = "201320"
        subject = "0"
        checkCourseList oldSections = do
            response <- liftIO $ fetchCourseList semester subject
            case response of
                Left err -> do
                    -- Record statuses as Unavailable.
                    updateWhere [SectionSemester ==. semester]
                                [SectionLastStatus =. Unavailable]
                    let recordUnavailable = recordHistory Unavailable
                        sectionIds = map entityKey $ Map.elems oldSections
                    mapM_ recordUnavailable sectionIds
                Right newSections -> do
                    return ()
        recordHistory status sectionId = do
            time <- liftIO $ getCurrentTime
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
