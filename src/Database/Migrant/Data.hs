module Database.Migrant.Data
  ( Migration (..)
  , Backend (..)
  , Message (..)
  , MigrateSettings (..)
  ) where

data Migration up down = Migration
  { migrationUp   :: up
  , migrationDown :: down
  , migrationDescription :: Maybe String
  } deriving (Eq, Show)

-- TODO is `e` necessary? All we do is show it
class (Eq q, Show q, Show e) => Backend b q e | b -> q e where
  backendEnsureStack         :: b -> IO Bool
  backendGetMigrations       :: b -> IO [Migration q (Maybe q)]
  backendBeginTransaction    :: b -> IO ()
  backendRollbackTransaction :: b -> IO ()
  backendCommitTransaction   :: b -> IO (Maybe e)
  backendRunMigration        :: b -> q -> IO (Maybe e)
  backendPushMigration       :: b -> Migration q (Maybe q) -> IO ()
  backendPopMigration        :: b -> IO ()

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
