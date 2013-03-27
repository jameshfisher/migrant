module Main where

import Test.Framework (defaultMain, testGroup, Test)
import Test.Framework.Providers.HUnit (testCase)

import Database.Migrant.Test.Backend.Mock (mockConnectExpectedState)

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests =
  [ testGroup "HUnit group 1"
      [ testCase "Expected connect" mockConnectExpectedState
      ]
  ]