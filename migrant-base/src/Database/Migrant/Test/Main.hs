module Main where

import Test.Framework (defaultMain, testGroup, Test)

import Database.Migrant.Test.Backend.Mock as Mock (testGroupBackendMock)
import Database.Migrant.Test.Backend.MockTwo as MockTwo (testGroupBackendMock)

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests =
  [ testGroup "Mock backend" Mock.testGroupBackendMock
  , testGroup "Mock backend two" MockTwo.testGroupBackendMock
  ]