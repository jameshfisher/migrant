module Database.Migrant.Backend.MockTwo where

import Control.Applicative
import Control.Monad.State

import Database.Migrant.Types.Migration (Migration (..))
import Database.Migrant.Types.Backend (Backend (..))

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

mockConnect :: MockConnection
mockConnect = MockConnection {
  mockConnectionLog = [],
  mockConnectionStack = Nothing,
  mockConnectionState = MockState Nothing 0
  }

lg :: Action -> State MockConnection ()
lg a = do
  conn <- get
  put $ conn { mockConnectionLog = (mockConnectionLog conn) ++ [a] }

instance Backend () where
  type BackendMonad () = State MockConnection
  type BackendQuery () = MockQuery
  type BackendCond  () = MockCondition

  backendEnsureStack conn = do
    lg ActionEnsureStack
    db <- get
    let maybeStack = mockConnectionStack db
    case maybeStack of
      Nothing -> do
        put $ db { mockConnectionStack = Just [] }
        return True
      Just _  -> return False

  backendGetMigrations conn = do
    lg ActionGetMigrations
    db <- get
    let maybeStack = mockConnectionStack db
    case maybeStack of
      Nothing    -> error "MockConnection: tried to get migrations when no stack exists!"
      Just stack -> return stack

  backendBeginTransaction conn = do
    lg ActionBeginTransaction
    db <- get
    let MockState rollback state = mockConnectionState db
    case rollback of
      Just _  -> error "MockConnection: tried to begin a transaction when inside a transaction"
      Nothing -> put $ db { mockConnectionState = MockState (Just state) state }

  backendRollbackTransaction conn = do
    lg ActionRollbackTransaction
    db <- get
    let MockState rollback _ = mockConnectionState db
    case rollback of
      Nothing -> error "MockConnection: tried to rollback a transaction when outside a transaction"
      Just r  -> put $ db { mockConnectionState = MockState Nothing r }

  backendCommitTransaction conn = do
    lg ActionCommitTransaction
    db <- get
    let MockState rollback state = mockConnectionState db
    case rollback of
      Nothing -> error "MockConnection: tried to commit a transaction when outside a transaction"
      Just _  -> do
        put $ db { mockConnectionState = MockState Nothing state }
        return Nothing

  backendRunMigration conn m = do
    lg $ ActionRunMigration m
    case m of
      Nothing -> return $ Just "MockConnection: invalid query"
      Just downValid -> do
        db <- get
        let state = mockConnectionState db
        put $ db {
          mockConnectionState = state { mockState = mockState state + downValid }
          }
        return Nothing

  backendPushMigration conn m = do
    lg $ ActionPushMigration m
    db <- get
    let maybeStack = mockConnectionStack db
    case maybeStack of
      Nothing    -> error "MockConnection: tried to push migration when no stack exists!"
      Just stack -> put $ db { mockConnectionStack = Just (m:stack) }

  backendPopMigration conn = do
    lg ActionPopMigration
    db <- get
    let maybeStack = mockConnectionStack db
    case maybeStack of
      Nothing    -> error "MockConnection: tried to pop migration when no stack exists!"
      Just stack -> case stack of
                      []   -> error "MockConnection: tried to pop migration when the stack is empty!"
                      _:ms -> put $ db { mockConnectionStack = Just ms }

  backendTestCondition conn cond = do
    lg $ ActionTestCondition cond
    MockState _ state <- mockConnectionState <$> get
    return $ cond <= state
