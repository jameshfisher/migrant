module Database.PostgreSQL.Migrate.Data
  ( Migration (..)
  , UpMigration (..)
  , BiMigration (..)
  , Backend (..)
  , MigrateSettings (..)
  ) where

data Migration = Migration
  { migrationName :: String
  , migrationUp   :: String
  , migrationDown :: Maybe String
  } deriving (Show)

data UpMigration = UpMigration
  { upMigrationName :: String
  , upMigrationUp   :: String
  } deriving (Show)

data BiMigration = BiMigration
  { biMigrationName :: String
  , biMigrationUp   :: String
  , biMigrationDown :: String
  } deriving (Show)

class Backend b where
  backendStackExists   :: b -> IO Bool
  backendCreateStack   :: b -> IO ()
  backendGetMigrations :: b -> IO [Migration]
  backendDownMigrate   :: b -> BiMigration -> IO ()
  backendUpMigrate     :: b -> Migration -> IO ()

data Backend b => MigrateSettings b = MigrateSettings
  { migrateSettingsBackend     :: b
  , migrateSettingsInteractive :: Bool
  }
