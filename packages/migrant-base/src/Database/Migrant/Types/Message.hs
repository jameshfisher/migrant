module Database.Migrant.Types.Message
  ( Message (..)
  ) where

data Message
  = MessageCreatedMigrationStack
  | MessageMigrationStartedUp String
  | MessageMigrationStartedDown String
  | MessageStartedUpQuery String
  | MessageStartedDownQuery String
  | MessageMigrationCommitted
  | MessageMigrationRolledBack String
  | MessageWarnNoDownMigration
  | MessageTestingDownMigration
  | MessageWarnNoPrecondition
  | MessageTestingPrecondition String
  | MessageWarnNoPostCondition
  | MessageTestingPostcondition String
  | MessageMissingDownMigrations [String]
  | MessageAborted
  | MessageCompleted Int
  deriving (Eq, Show)
