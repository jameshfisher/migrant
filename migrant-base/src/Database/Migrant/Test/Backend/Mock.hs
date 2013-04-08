module Database.Migrant.Test.Backend.Mock (testGroupBackendMock) where

import Data.Maybe (isJust, isNothing, fromJust)

import Control.Monad.State

import Test.Framework (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Test.QuickCheck ((==>))

import Database.Migrant.Types.Mock (MockConnection (..), MockState (..), Action (..))
import Database.Migrant.Types.Backend (Backend (..))
import Database.Migrant.Test.Types ()

import Database.Migrant.Backend.Mock ()

testGroupBackendMock :: [Test]
testGroupBackendMock =
  [ testGroup "backendEnsureStack"
    [ testGroup "on DB without stack"
      [ testProperty "returns True" $
          \conn ->
            isNothing (mockConnectionStack conn) ==>
              fst $ runState (backendEnsureStack ()) conn

      , testProperty "creates new stack" $
          \conn ->
            isNothing (mockConnectionStack conn) ==>
              mockConnectionStack (execState (backendEnsureStack ()) conn)
              == Just []
      ]

    , testGroup "on DB with stack"
      [ testProperty "returns False" $
          \conn ->
            isJust (mockConnectionStack conn) ==>
              not $ evalState (backendEnsureStack ()) conn

      , testProperty "leaves stack alone" $
          \conn ->
            isJust (mockConnectionStack conn) ==>
              mockConnectionStack (execState (backendEnsureStack ()) conn)
              == mockConnectionStack conn
      ]

    , testProperty "does not touch state" $
        \conn ->
          mockConnectionState (execState (backendEnsureStack ()) conn)
          == mockConnectionState conn

    , testProperty "logs action" $
        \conn ->
          mockConnectionLog (execState (backendEnsureStack ()) conn)
          == mockConnectionLog conn ++ [ActionEnsureStack]
    ]

  , testGroup "backendGetMigrations"
    [ testProperty "returns migrations" $
        \conn ->
          isJust (mockConnectionStack conn) ==>
            evalState (backendGetMigrations ()) conn
            == fromJust (mockConnectionStack conn)

    , testProperty "does not touch state" $
        \conn ->
          isJust (mockConnectionStack conn) ==>
            mockConnectionState (execState (backendGetMigrations ()) conn)
            == mockConnectionState conn

    , testProperty "logs action" $
        \conn ->
          isJust (mockConnectionStack conn) ==>
            mockConnectionLog (execState (backendGetMigrations ()) conn)
            == mockConnectionLog conn ++ [ActionGetMigrations]
    ]

  , testGroup "backendRunQuery"
    [ testGroup "with invalid query"
      [ testProperty "returns exception" $
          \conn ->
            isJust (mockConnectionStack conn) ==>
              isJust $ evalState (backendRunQuery () Nothing) conn

      , testProperty "does not touch state" $
          \conn ->
          isJust (mockConnectionStack conn) ==>
            mockConnectionState (execState (backendRunQuery () Nothing) conn)
            == mockConnectionState conn
      ]

    , testGroup "with valid query"
      [ testProperty "does not return exception" $
          \conn q ->
            isJust (mockConnectionStack conn) && isJust q ==>
              isNothing $ evalState (backendRunQuery () q) conn

      , testProperty "runs query" $
          \conn q ->
            isJust (mockConnectionStack conn) && isJust q ==>
              mockState (mockConnectionState $ execState (backendRunQuery () q) conn)
              == fromJust q + mockState (mockConnectionState conn)
      ]

    , testProperty "does not touch stack" $
        \conn ->
          isJust (mockConnectionStack conn) ==>
            mockConnectionStack (execState (backendRunQuery () Nothing) conn)
            == mockConnectionStack conn

    , testProperty "logs action" $
        \conn ->
          isJust (mockConnectionStack conn) ==>
            mockConnectionLog (execState (backendRunQuery () Nothing) conn)
            == mockConnectionLog conn ++ [ActionRunMigration Nothing]
    ]
  ]
