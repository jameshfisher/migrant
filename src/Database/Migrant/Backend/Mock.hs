module Database.Migrant.Backend.Mock where

import Data.IORef

import Database.Migrant.Data

data MockState = MockState
  { mockRollback :: Maybe Int
  , mockState    :: Int
  } deriving (Eq, Show)

type MockQuery = Maybe Int -- semantics: Nothing is invalid query; (Just i) adds i to state

type MockStack = Maybe [Migration MockQuery] -- head is latest

data MockConnection = MockConnection {
  mockConnectionStack :: MockStack,
  mockConnectionState :: MockState
  } deriving (Eq, Show)

mockConnect :: IO (IORef MockConnection)
mockConnect = newIORef $ MockConnection {
  mockConnectionStack = Nothing,
  mockConnectionState = MockState Nothing 0
  }

instance Backend (IORef MockConnection) MockQuery String where
  
  backendEnsureStack conn = do
    db <- readIORef conn
    let maybeStack = mockConnectionStack db
    case maybeStack of
      Nothing -> do
        writeIORef conn $ db { mockConnectionStack = Just [] }
        return True
      Just _  -> return False

  backendGetMigrations conn = do
    db <- readIORef conn
    let maybeStack = mockConnectionStack db
    case maybeStack of
      Nothing    -> error "MockConnection: tried to get migrations when no stack exists!"
      Just stack -> return stack

  backendBeginTransaction conn = do
    db <- readIORef conn
    let MockState rollback state = mockConnectionState db
    case rollback of
      Just _  -> error "MockConnection: tried to begin a transaction when inside a transaction"
      Nothing -> writeIORef conn $ db { mockConnectionState = MockState (Just state) state }

  backendRollbackTransaction conn = do
    db <- readIORef conn
    let MockState rollback _ = mockConnectionState db
    case rollback of
      Nothing -> error "MockConnection: tried to rollback a transaction when outside a transaction"
      Just r  -> writeIORef conn $ db { mockConnectionState = MockState Nothing r }

  backendCommitTransaction conn = do
    db <- readIORef conn
    let MockState rollback state = mockConnectionState db
    case rollback of
      Nothing -> error "MockConnection: tried to commit a transaction when outside a transaction"
      Just _  -> do
        writeIORef conn $ db { mockConnectionState = MockState Nothing state }
        return Nothing

  backendRunMigration conn m = case m of
    Nothing -> return $ Just "MockConnection: invalid query"
    Just downValid -> do
      db <- readIORef conn
      let state = mockConnectionState db
      writeIORef conn $ db {
        mockConnectionState = state { mockState = mockState state + downValid }
        }
      return Nothing

  backendPushMigration conn m = do
    db <- readIORef conn
    let maybeStack = mockConnectionStack db
    case maybeStack of
      Nothing    -> error "MockConnection: tried to push migration when no stack exists!"
      Just stack -> writeIORef conn $ db { mockConnectionStack = Just (m:stack) }

  backendPopMigration conn = do
    db <- readIORef conn
    let maybeStack = mockConnectionStack db
    case maybeStack of
      Nothing    -> error "MockConnection: tried to pop migration when no stack exists!"
      Just stack -> case stack of
                      []   -> error "MockConnection: tried to pop migration when the stack is empty!"
                      _:ms -> writeIORef conn $ db { mockConnectionStack = Just ms }
