module Database.Migrant.Backend.Mock where

import Data.IORef

import Database.Migrant.Data

type MockState = Int

type MockQuery = Maybe Int -- semantics: Nothing is invalid query; (Just i) adds i to state

type MockStack = Maybe [Migration MockQuery] -- head is latest

data MockConnection = MockConnection {
  mockConnectionStack :: MockStack,
  mockConnectionState :: MockState
  }

mockConnect :: IO (IORef MockConnection)
mockConnect = newIORef $ MockConnection {
  mockConnectionStack = Nothing,
  mockConnectionState = 0
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

  backendDownMigrate conn (BiMigration up down _) = case down of
    Nothing -> return $ Just "MockConnection: invalid query for down-migration"
    Just downValid -> do
      db <- readIORef conn
      let maybeStack = mockConnectionStack db
      case maybeStack of
        Nothing    -> error "MockConnection: tried to down-migrate when no stack exists!"
        Just stack -> case stack of
                        []   -> error "MockConnection: tried to down-migrate when the stack is empty!"
                        m:ms -> if migrationUp m /= up
                                  then error "MockConnection: tried to down-migrate a migration that isn't the top of the stack"
                                  else do
                                    writeIORef conn $ db {
                                      mockConnectionStack = Just ms,
                                      mockConnectionState = (mockConnectionState db) - downValid
                                      }
                                    return Nothing

  backendUpMigrate conn m@(Migration up _ _) = case up of
    Nothing -> return $ Just "MockConnection: invalid query for up-migration"
    Just upValid -> do      
      db <- readIORef conn
      let maybeStack = mockConnectionStack db
      case maybeStack of
        Nothing    -> error "MockConnection: tried to up-migrate when no stack exists!"
        Just stack -> do
                        writeIORef conn $ db {
                          mockConnectionStack = Just (m:stack),
                          mockConnectionState = (mockConnectionState db) + upValid
                          }
                        return Nothing
