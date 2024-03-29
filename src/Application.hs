{-# OPTIONS_GHC -fno-warn-orphans #-}
module Application
    ( makeApplication
    , getApplicationDev
    , makeFoundation
    , getAppConfig
    , getPersistConfig
    ) where

import Import
import Settings
import Yesod.Default.Config
import Yesod.Default.Main
import Yesod.Default.Handlers
import Network.Wai.Middleware.RequestLogger
import qualified Database.Persist.Store
import Database.Persist.GenericSql (runMigration)
import Network.HTTP.Conduit (newManager, def)
import Control.Monad.Logger (runLoggingT)
import System.IO (stdout)
import System.Log.FastLogger (mkLogger)

-- Import all relevant handler modules here.
-- Don't forget to add new modules to your cabal file!
import Handler.Admin
import Handler.Auth
import Handler.Home
import Handler.Info
import Handler.Order
import Handler.Verify

-- This line actually creates our YesodDispatch instance. It is the second half
-- of the call to mkYesodData which occurs in Foundation.hs. Please see the
-- comments there for more details.
mkYesodDispatch "App" resourcesApp

-- This function allocates resources (such as a database connection pool),
-- performs initialization and creates a WAI application. This is also the
-- place to put your migrate statements to have automatic database
-- migrations handled by Yesod.
makeApplication :: AppConfig DefaultEnv Extra -> IO Application
makeApplication conf = do
    foundation <- makeFoundation conf

    -- Initialize the logging middleware
    logWare <- mkRequestLogger def
        { outputFormat =
            if development
                then Detailed True
                else Apache FromSocket
        , destination = Logger $ appLogger foundation
        }

    -- Create the WAI application and apply middlewares
    app <- toWaiAppPlain foundation
    return $ logWare app

makeFoundation :: AppConfig DefaultEnv Extra -> IO App
makeFoundation conf = do
    manager <- newManager def
    s <- staticSite
    dbconf <- getPersistConfig conf
    --dbconf <- withYamlEnvironment "config/postgresql.yml" (appEnv conf)
    --          Database.Persist.Store.loadConfig >>=
    --          Database.Persist.Store.applyEnv
    p <- Database.Persist.Store.createPoolConfig (dbconf :: Settings.PersistConfig)
    logger <- mkLogger True stdout
    let foundation = App conf s p manager dbconf logger

   -- Perform database migration using our application's logging settings.
    runLoggingT
        (Database.Persist.Store.runPool dbconf (runMigration migrateAll) p)
        (messageLoggerSource foundation logger)

    return foundation

getAppConfig :: DefaultEnv -> IO (AppConfig DefaultEnv Extra)
getAppConfig env = loadConfig conf
    where 
        conf = (configSettings env)
            { csParseExtra = parseExtra
            , csFile = \e -> return $ case e of
                Development -> "config/settings.yml"
                _ -> "/etc/bannerstalker/settings.yml"
            }

getPersistConfig :: AppConfig DefaultEnv Extra -> IO PersistConfig
getPersistConfig conf =
    withYamlEnvironment filepath env
    Database.Persist.Store.loadConfig >>= Database.Persist.Store.applyEnv
    where
        env = appEnv conf
        filepath = case env of
            Development -> "config/postgresql.yml"
            _ -> "/etc/bannerstalker/postgresql.yml"
        

-- for yesod devel
getApplicationDev :: IO (Int, Application)
getApplicationDev =
    defaultDevelApp loader makeApplication
  where
    loader = loadConfig (configSettings Development)
        { csParseExtra = parseExtra
        }

getHumansR :: GHandler s m RepPlain
getHumansR = sendFile "text/plain;charset=utf-8" "config/humans.txt"
