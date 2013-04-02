module Main where

import Test.Framework (defaultMain, testGroup, Test)

import Database.Migrant.Test.Backend.SQLite (testGroupBackendSQLite)

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests =
  [ testGroup "SQLite backend" testGroupBackendSQLite
  ]