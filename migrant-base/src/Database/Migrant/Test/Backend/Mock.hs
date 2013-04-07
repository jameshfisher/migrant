module Database.Migrant.Test.Backend.Mock (testGroupBackendMock) where

import Data.Maybe (isJust, isNothing, fromJust)

import Control.Monad.State

import Test.Framework (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Test.QuickCheck ((==>))

import Database.Migrant.Types.Backend (Backend (..))
import Database.Migrant.Test.Types ()

import Database.Migrant.Backend.Mock

testGroupBackendMock :: [Test]
testGroupBackendMock =
  [ testGroup "backendEnsureStack"
    [ testGroup "on DB without stack"
      [ testProperty "returns True" $
          \conn ->
            (isNothing $ mockConnectionStack conn) ==>
              fst $ runState (backendEnsureStack ()) conn

      , testProperty "creates new stack" $
          \conn ->
            (isNothing $ mockConnectionStack conn) ==>
              (mockConnectionStack $ snd $ runState (backendEnsureStack ()) conn)
              == (Just [])
      ]

    , testGroup "on DB with stack"
      [ testProperty "returns False" $
          \conn ->
            (isJust $ mockConnectionStack conn) ==>
              not $ fst $ runState (backendEnsureStack ()) conn

      , testProperty "leaves stack alone" $
          \conn ->
            (isJust $ mockConnectionStack conn) ==>
              (mockConnectionStack $ snd $ runState (backendEnsureStack ()) conn)
              == mockConnectionStack conn
      ]

    , testProperty "does not touch state" $
        \conn ->
          (mockConnectionState $ snd $ runState (backendEnsureStack ()) conn)
          == mockConnectionState conn

    , testProperty "logs action" $
        \conn ->
          (mockConnectionLog $ snd $ runState (backendEnsureStack ()) conn)
          == mockConnectionLog conn ++ [ActionEnsureStack]
    ]

  , testGroup "backendGetMigrations"
    [ testProperty "returns migrations" $
        \conn ->
          (isJust $ mockConnectionStack conn) ==>
            (fst $ runState (backendGetMigrations ()) conn)
            == (fromJust $ mockConnectionStack conn)

    , testProperty "does not touch state" $
        \conn ->
          (isJust $ mockConnectionStack conn) ==>
            (mockConnectionState $ snd $ runState (backendGetMigrations ()) conn)
            == mockConnectionState conn

    , testProperty "logs action" $
        \conn ->
          (isJust $ mockConnectionStack conn) ==>
            (mockConnectionLog $ snd $ runState (backendGetMigrations ()) conn)
            == mockConnectionLog conn ++ [ActionGetMigrations]
    ]

  , testGroup "backendRunMigration"
    [ testGroup "with invalid query"
      [ testProperty "returns exception" $
          \conn ->
            (isJust $ mockConnectionStack conn) ==>
              isJust $ fst $ runState (backendRunMigration () Nothing) conn

      , testProperty "does not touch state" $
          \conn ->
          (isJust $ mockConnectionStack conn) ==>
            (mockConnectionState $ snd $ runState (backendRunMigration () Nothing) conn)
            == mockConnectionState conn
      ]

    , testGroup "with valid query"
      [ testProperty "does not return exception" $
          \conn q ->
            (isJust $ mockConnectionStack conn) && isJust q ==>
              isNothing $ fst $ runState (backendRunMigration () q) conn

      , testProperty "runs query" $
          \conn q ->
            (isJust $ mockConnectionStack conn) && isJust q ==>
              (mockState $ mockConnectionState $ snd $ runState (backendRunMigration () q) conn)
              == fromJust q + (mockState $ mockConnectionState conn)
      ]

    , testProperty "does not touch stack" $
        \conn ->
          (isJust $ mockConnectionStack conn) ==>
            (mockConnectionStack $ snd $ runState (backendRunMigration () Nothing) conn)
            == mockConnectionStack conn

    , testProperty "logs action" $
        \conn ->
          (isJust $ mockConnectionStack conn) ==>
            (mockConnectionLog $ snd $ runState (backendRunMigration () Nothing) conn)
            == mockConnectionLog conn ++ [ActionRunMigration Nothing]
    ]
  ]
