module Database.Migrant.Types.Message
  ( Message (..)
  ) where

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
