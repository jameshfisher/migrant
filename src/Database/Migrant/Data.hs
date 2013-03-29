module Database.Migrant.Data
  ( Migration (..)
  , Backend (..)
  , Message (..)
  , MigrateSettings (..)
  ) where

data Migration up down cond = Migration
  { migrationUp   :: up
  , migrationDown :: down
  , migrationPre  :: Maybe cond
  , migrationPost :: Maybe cond
  , migrationDescription :: Maybe String
  } deriving (Eq, Show)

-- TODO is `e` necessary? All we do is show it
class (Eq q, Show q, Eq cond, Show cond, Show e) => Backend conn q cond e | conn -> q cond e where
  backendEnsureStack         :: conn -> IO Bool
  backendGetMigrations       :: conn -> IO [Migration q (Maybe q) cond]
  backendBeginTransaction    :: conn -> IO ()
  backendRollbackTransaction :: conn -> IO ()
  backendCommitTransaction   :: conn -> IO (Maybe e)
  backendRunMigration        :: conn -> q -> IO (Maybe e)
  backendPushMigration       :: conn -> Migration q (Maybe q) cond -> IO ()
  backendPopMigration        :: conn -> IO ()
  backendTestCondition       :: conn -> cond -> IO (Maybe e)

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

data Backend conn q e => MigrateSettings conn q e = MigrateSettings
  { migrateSettingsBackend  :: conn
  , migrateSettingsFrontend :: Message -> IO ()
  }
