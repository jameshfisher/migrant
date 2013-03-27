module Database.Migrant.Test.Backend.Mock where

import Data.IORef
import Test.HUnit (Test(TestCase), assertEqual)

import Database.Migrant.Backend.Mock 

mockConnectExpectedState :: Test
mockConnectExpectedState = TestCase $ do
  conn <- mockConnect
  db <- readIORef conn
  assertEqual "" (MockConnection Nothing (MockState Nothing 0)) db

tests = mockConnectExpectedState