module Database.Migrant.Test.Backend.Mock (testGroupBackendMock) where

import Data.IORef
import qualified Test.HUnit as HUnit (assertEqual)
import Test.Framework (Test)
import Test.Framework.Providers.HUnit (testCase)

import Database.Migrant.Data
import Database.Migrant.Backend.Mock

mockConnectExpectedState :: IO ()
mockConnectExpectedState = do
  conn <- mockConnect
  db <- readIORef conn
  HUnit.assertEqual "" (MockConnection Nothing (MockState Nothing 0)) db

backendEnsureStackOnNewDB :: IO ()
backendEnsureStackOnNewDB = do
  conn <- mockConnect
  true <- backendEnsureStack conn
  db <- readIORef conn
  HUnit.assertEqual "" True true
  HUnit.assertEqual "" (Just []) (mockConnectionStack db)


testGroupBackendMock :: [Test]
testGroupBackendMock =
  [ testCase "Expected connect" mockConnectExpectedState
  , testCase "Ensure stack on new DB creates new stack" backendEnsureStackOnNewDB
  ]
