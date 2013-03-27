module Database.Migrant.Test.Backend.Mock where

import Data.IORef
import qualified Test.HUnit as HUnit (assertEqual)

import Database.Migrant.Backend.Mock 

mockConnectExpectedState :: IO ()
mockConnectExpectedState = do
  conn <- mockConnect
  db <- readIORef conn
  HUnit.assertEqual "" (MockConnection Nothing (MockState Nothing 0)) db
