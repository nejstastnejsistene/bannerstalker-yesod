module Bannerstalkerd where

import Prelude
--import Database.Persist
import System.Posix.Daemonize

import CourseList

daemon = CreateDaemon {  privilegedAction = return ()
                      ,  program = const bannerstalkerd
                      ,  name = Just "bannerstalkerd"
                      ,  user = Just "bannerstalker"
                      ,  group = Just "bannerstalker"
                      ,  syslogOptions = []
                      ,  pidfileDirectory = Just "/var/run"
                      }

bannerstalkerd :: IO ()
bannerstalkerd = do
    courseList <- getCourseList "201320" "MATH"
    case courseList of
        Left err -> do
            putStrLn $  "Error fetching courselist: " ++ err
        Right sections -> do
            --persistIds <- mapM insert sections        
            putStrLn $ show sections
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
startDaemon :: IO ()
--startDaemon = serviced daemon
startDaemon = bannerstalkerd
