module Database.PostgreSQL.Migrate.Data
  ( Migration (..)
  , BiMigration (..)
  , Backend (..)
  , MigrateSettings (..)
  ) where

data Migration q = Migration
  { migrationName :: String
  , migrationUp   :: q
  , migrationDown :: Maybe q
  } deriving (Show)

data BiMigration q = BiMigration
  { biMigrationName :: String
  , biMigrationUp   :: q
  , biMigrationDown :: q
  } deriving (Show)

class Eq q => Backend b q | b -> q where
  backendStackExists   :: b -> IO Bool
  backendCreateStack   :: b -> IO ()
  backendGetMigrations :: b -> IO [Migration q]
  backendDownMigrate   :: b -> BiMigration q -> IO ()
  backendUpMigrate     :: b -> Migration q -> IO ()

data Backend b q => MigrateSettings b q = MigrateSettings
  { migrateSettingsBackend     :: b
  , migrateSettingsInteractive :: Bool
  }
