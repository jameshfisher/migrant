module Database.Migrant.Data
  ( Migration (..)
  , BiMigration (..)
  , Backend (..)
  , Message (..)
  , MigrateSettings (..)
  ) where

data Migration q = Migration
  { migrationUp   :: q
  , migrationDown :: Maybe q
  , migrationDescription :: Maybe String
  } deriving (Eq, Show)

data BiMigration q = BiMigration
  { biMigrationUp   :: q
  , biMigrationDown :: q
  , biMigrationDescription :: Maybe String
  } deriving (Eq, Show)

class (Eq q, Show q, Show e) => Backend b q e | b -> q e where
  backendEnsureStack         :: b -> IO Bool
  backendGetMigrations       :: b -> IO [Migration q]
  backendBeginTransaction    :: b -> IO ()
  backendCommitTransaction   :: b -> IO ()
  backendRollbackTransaction :: b -> IO ()
  backendDownMigrate         :: b -> BiMigration q -> IO (Maybe e)
  backendUpMigrate           :: b -> Migration q -> IO (Maybe e)

data Message
  = MessageCreatedMigrationStack
  | MessageMigrationStartedUp String
  | MessageMigrationStartedDown String
  | MessageMigrationCommitted
  | MessageMigrationRolledBack String
  | MessageWarnNoDownMigration
  | MessageTestingMigration
  | MessageMissingDownMigrations [String]
  | MessageAborted
  | MessageCompleted Int

data Backend b q e => MigrateSettings b q e = MigrateSettings
  { migrateSettingsBackend  :: b
  , migrateSettingsFrontend :: Message -> IO ()
  }
