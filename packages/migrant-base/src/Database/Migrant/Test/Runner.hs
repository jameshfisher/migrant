module Database.Migrant.Test.Runner (testGroupRunner) where

import Control.Monad.State (State, execState)

import Test.Framework (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Database.Migrant.Types.MigrateSettings (MigrateSettings (..))
import Database.Migrant.Types.Mock (Mig, MockConnection (..))
import Database.Migrant.Types.Backend ()

import Database.Migrant.Test.Types ()
import Database.Migrant.Runner (runMigrations)

import Database.Migrant.Frontend.Mock (frontendMock)

runMock :: [Mig] -> State MockConnection Bool
runMock = runMigrations (MigrateSettings () frontendMock)

testGroupRunner :: [Test]
testGroupRunner =
  [ testGroup "runMigrations"
    [ testGroup "with same migrations"
      [ testProperty "does not touch stack" $
          \conn -> case mockConnectionStack conn of
            Nothing -> True
            Just migs ->
              mockConnectionStack (execState (runMock migs) conn) == Just migs
      ]
    ]
  ]
