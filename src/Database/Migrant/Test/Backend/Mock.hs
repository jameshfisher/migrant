module Database.Migrant.Test.Backend.Mock (testGroupBackendMock) where

import Data.IORef
import qualified Test.HUnit as HUnit (assertEqual)
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)

import Database.Migrant.Data
import Database.Migrant.Backend.Mock

testGroupBackendMock :: [Test]
testGroupBackendMock =
  [ testCase "Expected connect" $ do
      conn <- mockConnect
      db <- readIORef conn
      HUnit.assertEqual
        "mockConnect should return a DB with no migration stack, a fresh DB state, and no transaction"
        (MockConnection Nothing (MockState Nothing 0))
        db

  , testGroup "backendEnsureStack"
    [ testCase "on new DB creates new stack" $ do
        conn <- mockConnect
        true <- backendEnsureStack conn
        HUnit.assertEqual "backendEnsureStack should return True when creating a stack" True true
        db <- readIORef conn
        HUnit.assertEqual "backendEnsureStack should create a migration stack when none exists" (Just []) (mockConnectionStack db)

    , testCase "on initialized DB does nothing" $ do
        let migs =  [ Migration (Just 4)    (Just $ Just (-4)) (Just "add 4")
                    , Migration (Just (-3)) (Just $ Just 3)    (Just "subtract 3")
                    ]
            initial = MockConnection (Just migs) (MockState Nothing 4)
        conn <- newIORef initial
        false <- backendEnsureStack conn
        HUnit.assertEqual "backendEnsureStack should return False when not creating a stack" False false
        db <- readIORef conn
        HUnit.assertEqual "backendEnsureStack should do nothing when a migration stack exists" initial db
    ]

  , testCase "backendGetMigrations returns migrations" $ do
      let migs =  [ Migration (Just 4)    (Just $ Just (-4)) (Just "add 4")
                  , Migration (Just (-3)) (Just $ Just 3)    (Just "subtract 3")
                  ]
          initial = MockConnection (Just migs) (MockState Nothing 4)
      conn <- newIORef initial
      migs' <- backendGetMigrations conn
      HUnit.assertEqual "backendGetMigrations returns migrations in current state" migs migs'
      db <- readIORef conn
      HUnit.assertEqual "backendGetMigrations does not change state" initial db

  , testGroup "backendDownMigrate"
    [ testCase "returns exception with invalid down-migration" $ do
        let initial = (MockConnection (Just [Migration (Just 4) (Just Nothing) (Just "add 4")]) (MockState Nothing 4))
        conn <- newIORef initial
        res <- backendDownMigrate conn $ BiMigration (Just 4) Nothing (Just "add 4")
        HUnit.assertEqual "" (Just "MockConnection: invalid query for down-migration") res
        db <- readIORef conn
        HUnit.assertEqual "" initial db

    , testCase "runs valid down-migration" $ do
        conn <- newIORef (MockConnection (Just [Migration (Just 4) (Just (Just (-4))) (Just "add 4")]) (MockState Nothing 4))
        res <- backendDownMigrate conn $ BiMigration (Just 4) (Just (-4)) (Just "add 4")
        HUnit.assertEqual "" Nothing res
        db <- readIORef conn
        HUnit.assertEqual "" (MockConnection (Just []) (MockState Nothing 0)) db
    ]

  , testGroup "backendUpMigrate"
    [ testCase "returns exception with invalid up-migration" $ do
        let initial = (MockConnection (Just []) (MockState Nothing 0))
        conn <- newIORef initial
        res <- backendUpMigrate conn $ Migration Nothing Nothing (Just "add 4")
        HUnit.assertEqual "" (Just "MockConnection: invalid query for up-migration") res
        db <- readIORef conn
        HUnit.assertEqual "" initial db

    , testCase "runs valid up-migration" $ do
        conn <- newIORef (MockConnection (Just []) (MockState Nothing 0))
        res <- backendUpMigrate conn $ Migration (Just 4) (Just (Just (-4))) (Just "add 4")
        HUnit.assertEqual "" Nothing res
        db <- readIORef conn
        HUnit.assertEqual "" (MockConnection (Just [Migration (Just 4) (Just (Just (-4))) (Just "add 4")]) (MockState Nothing 4)) db
    ]
  ]
