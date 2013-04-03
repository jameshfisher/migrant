module Database.Migrant.Types.MigrateSettings
  ( MigrateSettings (..)
  ) where

import Database.Migrant.Types.Backend
import Database.Migrant.Types.Message

data Backend conn q cond m => MigrateSettings conn q cond m = MigrateSettings
  { migrateSettingsBackend  :: conn
  , migrateSettingsFrontend :: Message -> m ()
  }
