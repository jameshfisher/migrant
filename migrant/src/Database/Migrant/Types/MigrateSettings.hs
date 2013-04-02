module Database.Migrant.Types.MigrateSettings
  ( MigrateSettings (..)
  ) where

import Database.Migrant.Types.Backend
import Database.Migrant.Types.Message

data Backend conn q cond => MigrateSettings conn q cond = MigrateSettings
  { migrateSettingsBackend  :: conn
  , migrateSettingsFrontend :: Message -> IO ()
  }
