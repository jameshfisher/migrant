module Database.Migrant.Types.MigrateSettings
  ( MigrateSettings (..)
  ) where

import Database.Migrant.Types.Backend
import Database.Migrant.Types.Message

data Backend conn => MigrateSettings conn = MigrateSettings
  { migrateSettingsBackend  :: conn
  , migrateSettingsFrontend :: Message -> BackendMonad conn ()
  }
