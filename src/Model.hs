module Model where

import Prelude
import Yesod
import Data.Text (Text)
import Data.Time (UTCTime)
import Database.Persist.Quasi
import Text.Blaze (ToMarkup, toMarkup)
import Text.Blaze.Internal (string)

data SectionStatus = Open | Closed | Unavailable
    deriving (Read, Show, Eq)
derivePersistField "SectionStatus"

-- Allow us to interpolate SectionStatuses in hamlet templates.
instance ToMarkup SectionStatus where
    toMarkup = string . show

-- You can define all of your database entities in the entities file.
-- You can find more information on persistent and how to declare entities
-- at:
-- http://www.yesodweb.com/book/persistent/
share [mkPersist sqlOnlySettings, mkMigrate "migrateAll"]
    $(persistFileWith lowerCaseSettings "config/models")
