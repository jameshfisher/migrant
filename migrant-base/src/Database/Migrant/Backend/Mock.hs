{-# OPTIONS_GHC -fno-warn-orphans #-}
module Database.Migrant.Backend.Mock where

import Control.Applicative
import Control.Monad.State

import Database.Migrant.Types.Backend (Backend (..))
import Database.Migrant.Types.Mock (MockConnection (..), MockState (..), MockQuery, MockCondition, Action (..))

mockConnect :: MockConnection
mockConnect = MockConnection {
  mockConnectionLog = [],
  mockConnectionStack = Nothing,
  mockConnectionState = MockState Nothing 0
  }

lg :: Action -> State MockConnection ()
lg a = do
  conn <- get
  put $ conn { mockConnectionLog = mockConnectionLog conn ++ [a] }

instance Backend () where
  type BackendMonad () = State MockConnection
  type BackendQuery () = MockQuery
  type BackendCond  () = MockCondition

  backendEnsureStack _ = do
    lg ActionEnsureStack
    db <- get
    let maybeStack = mockConnectionStack db
    case maybeStack of
      Nothing -> do
        put $ db { mockConnectionStack = Just [] }
        return True
      Just _  -> return False

  backendGetMigrations _ = do
    lg ActionGetMigrations
    db <- get
    let maybeStack = mockConnectionStack db
    case maybeStack of
      Nothing    -> error "MockConnection: tried to get migrations when no stack exists!"
      Just stack -> return stack

  backendBeginTransaction _ = do
    lg ActionBeginTransaction
    db <- get
    let MockState rollback state = mockConnectionState db
    case rollback of
      Just _  -> error "MockConnection: tried to begin a transaction when inside a transaction"
      Nothing -> put $ db { mockConnectionState = MockState (Just state) state }

  backendRollbackTransaction _ = do
    lg ActionRollbackTransaction
    db <- get
    let MockState rollback _ = mockConnectionState db
    case rollback of
      Nothing -> error "MockConnection: tried to rollback a transaction when outside a transaction"
      Just r  -> put $ db { mockConnectionState = MockState Nothing r }

  backendCommitTransaction _ = do
    lg ActionCommitTransaction
    db <- get
    let MockState rollback state = mockConnectionState db
    case rollback of
      Nothing -> error "MockConnection: tried to commit a transaction when outside a transaction"
      Just _  -> do
        put $ db { mockConnectionState = MockState Nothing state }
        return Nothing

  backendRunMigration _ m = do
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

  backendPushMigration _ m = do
    lg $ ActionPushMigration m
    db <- get
    let maybeStack = mockConnectionStack db
    case maybeStack of
      Nothing    -> error "MockConnection: tried to push migration when no stack exists!"
      Just stack -> put $ db { mockConnectionStack = Just (m:stack) }

  backendPopMigration _ = do
    lg ActionPopMigration
    db <- get
    let maybeStack = mockConnectionStack db
    case maybeStack of
      Nothing    -> error "MockConnection: tried to pop migration when no stack exists!"
      Just stack -> case stack of
                      []   -> error "MockConnection: tried to pop migration when the stack is empty!"
                      _:ms -> put $ db { mockConnectionStack = Just ms }

  backendTestCondition _ cond = do
    lg $ ActionTestCondition cond
    MockState _ state <- mockConnectionState <$> get
    return $ if cond <= state then Nothing else Just "MockConnection: condition failed"
