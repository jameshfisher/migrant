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

class (Eq q, Show q, Eq cond, Show cond) => Backend conn q cond | conn -> q cond where
  backendEnsureStack         :: conn -> IO Bool
  backendGetMigrations       :: conn -> IO [Migration q (Maybe q) cond]
  backendBeginTransaction    :: conn -> IO ()
  backendRollbackTransaction :: conn -> IO ()
  backendCommitTransaction   :: conn -> IO (Maybe String)
  backendRunMigration        :: conn -> q -> IO (Maybe String)
  backendPushMigration       :: conn -> Migration q (Maybe q) cond -> IO ()
  backendPopMigration        :: conn -> IO ()
  backendTestCondition       :: conn -> cond -> IO Bool

data Message
  = MessageCreatedMigrationStack
  | MessageMigrationStartedUp String
  | MessageMigrationStartedDown String
  | MessageMigrationCommitted
  | MessageMigrationRolledBack String
  | MessageWarnNoDownMigration
  | MessageTestingDownMigration
  | MessageWarnNoPrecondition
  | MessageTestingPrecondition
  | MessageWarnNoPostCondition
  | MessageTestingPostcondition
  | MessageMissingDownMigrations [String]
  | MessageAborted
  | MessageCompleted Int

data Backend conn q cond => MigrateSettings conn q cond = MigrateSettings
  { migrateSettingsBackend  :: conn
  , migrateSettingsFrontend :: Message -> IO ()
  }
