module Database.Migrant (module X) where

import Database.Migrant.Data as X (Migration(..), MigrateSettings(..))
import Database.Migrant.Runner as X (runMigrations)
