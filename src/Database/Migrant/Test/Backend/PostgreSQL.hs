{-# LANGUAGE QuasiQuotes #-}
module Database.Migrant.Test.Backend.PostgreSQL (testGroupBackendPostgreSQL) where

import Control.Monad (void)

import qualified Test.HUnit as HUnit (assertEqual)
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.SqlQQ

import Database.Migrant.Data
import Database.Migrant.Backend.PostgreSQL ()

testConnect :: IO Connection
testConnect = connect ConnectInfo
  { connectHost = "127.0.0.1"
  , connectPort = 5432
  , connectUser = "migrate"
  , connectPassword = "migrate"
  , connectDatabase = "migrate"
  }

clean :: Connection -> IO ()
clean conn = void $ execute_ conn
  [sql|
    drop schema if exists migrant cascade;
    drop table if exists public.foo cascade;
  |]

assertStack :: Connection -> String -> IO ()
assertStack conn s = do
  [[one]] <- query_ conn
    [sql|
      select count(*) from information_schema.tables t
        where t.table_schema = 'migrant'
          and t.table_name = 'migration'
    |]
  HUnit.assertEqual s (1 :: Int) one

testGroupBackendPostgreSQL :: [Test]
testGroupBackendPostgreSQL =
  [ testCase "Expected connect" $ do
      conn <- testConnect
      [[n]] <- query_ conn [sql| select (1 + 1) |]
      HUnit.assertEqual "testConnect should return a sane connection" (2 :: Int) n

  , testGroup "backendEnsureStack"
    [ testCase "on new DB creates new stack" $ do
        conn <- testConnect
        clean conn
        true <- backendEnsureStack conn
        HUnit.assertEqual "backendEnsureStack should return True when creating a stack" True true
        assertStack conn "backendEnsureStack should create a migration stack when none exists"

    , testCase "on initialized DB does nothing" $ do
        conn <- testConnect
        _ <- backendEnsureStack conn
        false <- backendEnsureStack conn
        HUnit.assertEqual "backendEnsureStack should return False when not creating a stack" False false
        assertStack conn "backendEnsureStack should do nothing when a migration stack exists"
    ]
  ]