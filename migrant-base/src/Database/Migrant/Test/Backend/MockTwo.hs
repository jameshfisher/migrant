module Database.Migrant.Test.Backend.MockTwo (testGroupBackendMock) where

import Control.Monad.State

import Test.Framework (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Database.Migrant.Types.Migration (Migration (..))
import Database.Migrant.Types.Backend (Backend (..))

import Database.Migrant.Backend.MockTwo

data Eq a => Tst a = Tst {
  tstPrecondition :: MockConnection,
  tstAction :: State MockConnection a,
  tstReturn :: a,
  tstPostcondition :: MockConnection
}

test :: Eq a => Tst a -> Bool
test (Tst pre act ret post) = (runState act pre) == (ret, post)

testGroupBackendMock :: [Test]
testGroupBackendMock =
  [ testGroup "backendEnsureStack"
    [ testProperty "on new DB creates new stack" $ test $ Tst {
        tstPrecondition  = mockConnect,
        tstAction        = backendEnsureStack (),
        tstReturn        = True,
        tstPostcondition = MockConnection {
                            mockConnectionLog = [ActionEnsureStack],
                            mockConnectionStack = Just [],
                            mockConnectionState = MockState Nothing 0
                            }
        }
    , testProperty "on initialized DB does nothing" $ test $ Tst {
        tstPrecondition  = MockConnection [ActionEnsureStack] (Just []) (MockState Nothing 0),
        tstAction        = backendEnsureStack (),
        tstReturn        = False,
        tstPostcondition = MockConnection [ActionEnsureStack, ActionEnsureStack] (Just []) (MockState Nothing 0)
        }

    , testProperty "backendGetMigrations returns migrations" $ test $ 
        let migs =  [ Migration (Just 4)    (Just $ Just (-4)) Nothing Nothing (Just "add 4")
                    , Migration (Just (-3)) (Just $ Just 3)    Nothing Nothing (Just "subtract 3")
                    ]
        in Tst {
          tstPrecondition  = MockConnection [] (Just migs) (MockState Nothing 4),
          tstAction        = backendGetMigrations (),
          tstReturn        = migs,
          tstPostcondition = MockConnection [ActionGetMigrations] (Just migs) (MockState Nothing 4)
          }

    , testGroup "backendRunMigration"
      [ testProperty "returns exception with invalid query" $ test $ Tst {
          tstPrecondition  = MockConnection [] (Just [Migration (Just 4) (Just Nothing) Nothing Nothing (Just "add 4")]) (MockState Nothing 4),
          tstAction        = backendRunMigration () Nothing,
          tstReturn        = Just "MockConnection: invalid query",
          tstPostcondition = MockConnection [ActionRunMigration Nothing] (Just [Migration (Just 4) (Just Nothing) Nothing Nothing (Just "add 4")]) (MockState Nothing 4)
          }

      , testProperty "runs valid query" $ test $ Tst {
          tstPrecondition  = MockConnection [] (Just []) (MockState Nothing 0),
          tstAction        = backendRunMigration () (Just 4),
          tstReturn        = Nothing,
          tstPostcondition = MockConnection [ActionRunMigration $ Just 4] (Just []) (MockState Nothing 4)
          }
      ]
    ]
  ]
