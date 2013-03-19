module Database.Migrant.Data
  ( Migration (..)
  , BiMigration (..)
  , Backend (..)
  , MigrateSettings (..)
  ) where

data Migration q = Migration
  { migrationUp   :: q
  , migrationDown :: Maybe q
  , migrationDescription :: Maybe String
  } deriving (Show)

data BiMigration q = BiMigration
  { biMigrationUp   :: q
  , biMigrationDown :: q
  , biMigrationDescription :: Maybe String
  } deriving (Show)

class (Eq q, Show q, Show e) => Backend b q e | b -> q e where
  backendStackExists   :: b -> IO Bool
  backendCreateStack   :: b -> IO ()
  backendGetMigrations :: b -> IO [Migration q]
  backendDownMigrate   :: b -> BiMigration q -> IO (Maybe e)
  backendUpMigrate     :: b -> Migration q -> IO (Maybe e)

data Backend b q e => MigrateSettings b q e = MigrateSettings
  { migrateSettingsBackend     :: b
  , migrateSettingsInteractive :: Bool
  }
