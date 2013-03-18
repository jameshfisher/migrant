module Database.PostgreSQL.Migrate.Data
  ( Migration (..)
  , UpMigration (..)
  , BiMigration (..)
  , Backend (..)
  , MigrateSettings (..)
  ) where

data Migration q = Migration
  { migrationName :: String
  , migrationUp   :: q
  , migrationDown :: Maybe q
  } deriving (Show)

data UpMigration q = UpMigration
  { upMigrationName :: String
  , upMigrationUp   :: q
  } deriving (Show)

data BiMigration q = BiMigration
  { biMigrationName :: String
  , biMigrationUp   :: q
  , biMigrationDown :: q
  } deriving (Show)

class Backend b where
  backendStackExists   :: b -> IO Bool
  backendCreateStack   :: b -> IO ()
  backendGetMigrations :: b -> IO [Migration String]
  backendDownMigrate   :: b -> BiMigration String -> IO ()
  backendUpMigrate     :: b -> Migration String -> IO ()

data Backend b => MigrateSettings b = MigrateSettings
  { migrateSettingsBackend     :: b
  , migrateSettingsInteractive :: Bool
  }
