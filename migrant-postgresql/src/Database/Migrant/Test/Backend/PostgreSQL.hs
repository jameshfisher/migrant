{-# LANGUAGE QuasiQuotes #-}
module Database.Migrant.Test.Backend.PostgreSQL (testGroupBackendPostgreSQL) where

import Data.Maybe (isJust)
import Control.Monad (void)

import qualified Test.HUnit as HUnit (assert, assertEqual)
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.SqlQQ

import Database.Migrant.Types.Migration (defaultMigration)
import Database.Migrant.Types.Backend (Backend (..))
import Database.Migrant.Backend.PostgreSQL (addColumn)

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
    rollback;
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
    [ testCase "on new DB" $ do
        conn <- testConnect
        clean conn

        true <- backendEnsureStack conn

        HUnit.assertEqual "returns True" True true

        assertStack conn "creates new stack"

        migs <- backendGetMigrations conn
        HUnit.assertEqual "creates new stack with no migrations" [] migs

    , testCase "on initialized DB does nothing" $ do
        conn <- testConnect
        clean conn

        _ <- backendEnsureStack conn
        false <- backendEnsureStack conn

        HUnit.assertEqual "backendEnsureStack should return False when not creating a stack" False false
        assertStack conn "backendEnsureStack should do nothing when a migration stack exists"
    ]
  , testGroup "backendGetMigrations"
    [ testCase "after creating migrations" $ do
        let migs = [ addColumn "public" "foo" "bar" "text", addColumn "public" "baz" "qux" "boolean", defaultMigration "insert into foo (bar) values ('ghres')"]

        conn <- testConnect
        clean conn

        _ <- backendEnsureStack conn

        mapM_ (backendPushMigration conn) migs

        migs' <- backendGetMigrations conn

        HUnit.assertEqual "returns those migrations" migs migs'
    ]
  , testGroup "backendRunMigration"
    [ testCase "on good migration" $ do
        conn <- testConnect
        clean conn

        merr <- backendRunMigration conn "create table foo (bar text)"

        HUnit.assertEqual "returns Nothing" Nothing merr

        [[one]] <- query_ conn
          [sql|
            select count(*) from information_schema.tables t
              where t.table_schema = 'public'
                and t.table_name = 'foo'
          |]
        HUnit.assertEqual "runs the migration" (1 :: Int) one

    , testCase "on bad migration" $ do
        conn <- testConnect
        clean conn

        merr <- backendRunMigration conn "ntsrbsgdx"      

        HUnit.assert $ isJust merr
    ]
  , testGroup "backendPopMigration"
    [ testCase "when migrations exist" $ do
        let migs = [ addColumn "public" "foo" "bar" "text", addColumn "public" "baz" "qux" "boolean"]

        conn <- testConnect
        clean conn

        _ <- backendEnsureStack conn

        mapM_ (backendPushMigration conn) migs

        backendPopMigration conn

        migs' <- backendGetMigrations conn

        HUnit.assertEqual "deletes the last migration" (init migs) migs'
    ]
  ]