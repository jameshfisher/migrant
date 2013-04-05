module Database.Migrant.Test.Backend.MockTwo (testGroupBackendMock) where

import Control.Monad.State

import Test.Framework (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)

--import Database.Migrant.Types.Migration (Migration (..))
import Database.Migrant.Types.Backend (Backend (..))

import Database.Migrant.Backend.MockTwo

testGroupBackendMock :: [Test]
testGroupBackendMock =
  [ testGroup "backendEnsureStack"
    [ testProperty "on new DB creates new stack"    $       fst $ runState (backendEnsureStack ()) mockConnect
    , testProperty "on initialized DB does nothing" $ not $ fst $ runState (backendEnsureStack ()) (MockConnection (Just []) (MockState Nothing 0))
    ]
  ]
