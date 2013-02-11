module Bannerstalkerd where

import Prelude
import Database.Persist
import Database.Persist.Postgresql
import System.Posix.Daemonize
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Resource (runResourceT)

import CourseList
import Model
import Settings

{-
daemon :: CreateDaemon ()
daemon = CreateDaemon {  privilegedAction = return ()
                      ,  program = const bannerstalkerd
                      ,  name = Just "bannerstalkerd"
                      ,  user = Just "bannerstalker"
                      ,  group = Just "bannerstalker"
                      ,  syslogOptions = []
                      ,  pidfileDirectory = Just "/var/run"
                      }
-}

bannerstalkerd :: IO ()
bannerstalkerd = do
    courseList <- fetchCourseList "201320" "MATH"
    case courseList of
        Left err -> do
            putStrLn $  "Error fetching courselist: " ++ err
        Right sections -> do
            let firstSection = head sections
            putStrLn $ show firstSection
            --persistId <- insert firstSection
            --putStrLn $ show persistId
    return ()

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

--main = serviced daemon
--main = bannerstalkerd
startDaemon :: PersistConfig -> IO ()
--startDaemon = serviced daemon
startDaemon conf = do
    --putStrLn "Started..."
    --config <- (fromArgs parseExtra)
    --app <- makeFoundation config
    --let pool =  (connPool app)
    --    dbconf = (persistConfig app)
    --putStrLn $ show pool
    --dbConf <- getPersistConfig conf
    putStrLn $ show $ pgConnStr conf
    {-
    runResourceT $ withPostgresqlConn "host=localhost port=5432 user=bannerstalker dbname=bannerstalker" $ runSqlConn $ do
        asdfId <- insert $ Section "a" 0 "b" "c" "d" "e" "f" "g" Closed
        fdsaId <- insert $ Section "z" 0 "b" "c" "d" "e" "f" "g" Open
        liftIO $ putStrLn $ show asdfId
        liftIO $ putStrLn $ show fdsaId
    -}
