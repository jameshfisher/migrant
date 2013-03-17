module Database.PostgreSQL.Migrate.Data
  ( Migration (..)
  , UpMigration (..)
  , BiMigration (..)
  , MigrateSettings (..)
  ) where

import Database.PostgreSQL.Simple (Connection)

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

data MigrateSettings = MigrateSettings
  { migrateSettingsConnection :: Connection
  }