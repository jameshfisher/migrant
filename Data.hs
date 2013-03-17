module Data where

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
