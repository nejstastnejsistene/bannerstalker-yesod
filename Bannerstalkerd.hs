module Bannerstalkerd where

import Import
import Prelude
import Database.Persist
import Database.Persist.Store
import Database.Persist.GenericSql
import Database.Persist.Postgresql
import System.Posix.Daemonize
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Resource (runResourceT)

import CourseList
import Model
import Settings

{-daemon :: CreateDaemon ()
daemon = CreateDaemon {  privilegedAction = return ()
                      ,  program = const bannerstalkerd
                      ,  name = Just "bannerstalkerd"
                      ,  user = Just "bannerstalker"
                      ,  group = Just "bannerstalker"
                      ,  syslogOptions = []
                      ,  pidfileDirectory = Just "/var/run"
                      }
-}

--bannerstalkerd :: (MonadIO m) => [Section] -> SqlPersist m ()
{-bannerstalkerd sections = do
    let firstSection = head sections
    sectionId <- insert firstSection    
    liftIO $ putStrLn $ show sectionId -}

    -- sections from db
    -- statuses from db
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

startDaemon :: Settings.PersistConfig -> IO ()
--startDaemon = serviced daemon
startDaemon conf = do
    putStrLn $ show $ pgConnStr conf
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
    {-pool <- createPoolConfig conf 
    courseList <- fetchCourseList "201320" "MATH"
    case courseList of
        Left err -> putStrLn "error"
        Right sections -> do
            let section = head section
            putStrLn $ show section
            runPool conf asdf-}

asdf section = do
    runMigration migrateAll
    insert section
    return ()
