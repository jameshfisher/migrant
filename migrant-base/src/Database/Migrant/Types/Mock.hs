module Database.Migrant.Types.Mock
  ( MockState (..)
  , MockQuery
  , MockCondition
  , Mig
  , MockStack
  , Action (..)
  , MockConnection (..)
  ) where

import Database.Migrant.Types.Migration (Migration)

data MockState = MockState
  { mockRollback :: Maybe Int
  , mockState    :: Int
  } deriving (Eq, Show)

type MockQuery = Maybe Int -- semantics: Nothing is invalid query; (Just i) adds i to state

type MockCondition = Int -- semantics: db state has to be >= this

type Mig = Migration MockQuery (Maybe MockQuery) MockCondition

type MockStack = Maybe [Mig] -- head is latest

data Action
  = ActionEnsureStack
  | ActionGetMigrations
  | ActionBeginTransaction
  | ActionRollbackTransaction
  | ActionCommitTransaction
  | ActionRunMigration MockQuery
  | ActionPushMigration Mig
  | ActionPopMigration
  | ActionTestCondition MockCondition
  deriving (Eq, Show)

data MockConnection = MockConnection {
  mockConnectionLog   :: [Action],
  mockConnectionStack :: MockStack,
  mockConnectionState :: MockState
  } deriving (Eq, Show)
