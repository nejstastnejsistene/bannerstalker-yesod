module Bannerstalkerd where 
import Import
import Prelude
import Database.Persist
import Database.Persist.Postgresql
import Control.Monad.Trans.Resource (runResourceT)

import CourseList
import Model
import Settings

bannerstalkerd :: PersistConfig -> IO ()
bannerstalkerd dbConf = do
    let conn = withPostgresqlConn (pgConnStr dbConf)
    runResourceT $ conn $ runSqlConn $ do 
        -- sections from db
        sections <- selectList [] []
        liftIO $ putStrLn $ show (sections :: [Entity Section])
    return ()
    --sections <- runPool dbConf (do selectList [] []) pool
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
