module Model where

import Prelude
import Yesod
import Data.ByteString.Lazy.Char8
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

data PrivilegeLevel = Level1 | Level2 | Level3 | Admin
    deriving (Read, Show, Eq, Ord, Bounded, Enum)
derivePersistField "PrivilegeLevel"

data NotificationType = EmailNotification | SmsNotification
    deriving (Read, Show, Eq)
derivePersistField "NotificationType"

-- So we can use ByteString inside the datatbase.
derivePersistField "ByteString"

-- You can define all of your database entities in the entities file.
-- You can find more information on persistent and how to declare entities
-- at:
-- http://www.yesodweb.com/book/persistent/
share [mkPersist sqlOnlySettings, mkMigrate "migrateAll"]
    $(persistFileWith lowerCaseSettings "config/models")
