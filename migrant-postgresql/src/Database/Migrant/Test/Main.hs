module Main where

import Test.Framework (defaultMain, testGroup, Test)

import Database.Migrant.Test.Backend.PostgreSQL (testGroupBackendPostgreSQL)

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests =
  [ testGroup "PostgreSQL backend" testGroupBackendPostgreSQL
  ]