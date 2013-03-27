module Database.Migrant.Test.Backend.Mock (testGroupBackendMock) where

import Data.IORef
import qualified Test.HUnit as HUnit (assertEqual)
import Test.Framework (Test)
import Test.Framework.Providers.HUnit (testCase)

import Database.Migrant.Data
import Database.Migrant.Backend.Mock

testGroupBackendMock :: [Test]
testGroupBackendMock =
  [ testCase "Expected connect" $ do
      conn <- mockConnect
      db <- readIORef conn
      HUnit.assertEqual "" (MockConnection Nothing (MockState Nothing 0)) db

  , testCase "Ensure stack on new DB creates new stack" $ do
      conn <- mockConnect
      true <- backendEnsureStack conn
      db <- readIORef conn
      HUnit.assertEqual "" True true
      HUnit.assertEqual "" (Just []) (mockConnectionStack db)
  ]
