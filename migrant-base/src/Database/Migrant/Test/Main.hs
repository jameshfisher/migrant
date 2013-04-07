module Main where

import Test.Framework (defaultMain, testGroup, Test)

import Database.Migrant.Test.Backend.Mock (testGroupBackendMock)
import Database.Migrant.Test.PlanMigration (testGroupPlanMigration)

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests =
  [ testGroup "Mock backend" testGroupBackendMock
  , testGroup "Plan migration" testGroupPlanMigration
  ]