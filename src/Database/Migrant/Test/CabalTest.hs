module Database.Migrant.Test.CabalTest (tests) where

import qualified Distribution.TestSuite as Cabal
import qualified Test.HUnit as HUnit

-- both of the above libraries really suck a load of balls.

import Database.Migrant.Test.Backend.Mock (mockConnectExpectedState)

instance Cabal.TestOptions HUnit.Test where
  name _ = "HUnit"
  options _ = []
  defaultOptions _ = return $ Cabal.Options []
  check _ _ = []

instance Cabal.ImpureTestable HUnit.Test where
  runM t _ = do
    counts <- HUnit.runTestTT t
    case HUnit.errors counts of
      0 -> return Cabal.Pass
      _ -> return . Cabal.Fail $ show counts

tests :: [Cabal.Test]
tests = [Cabal.impure mockConnectExpectedState]
