{-# OPTIONS_GHC -fno-warn-orphans #-}
module Database.Migrant.Test.Types where

import Control.Applicative ((<$>), (<*>))

import Database.Migrant.Types.Migration (Migration (..))
import Database.Migrant.Backend.Mock
  ( Mig
  , MockState (..)
  , Action (..)
  , MockConnection (..)
  )

import Test.QuickCheck (choose)
import Test.QuickCheck.Arbitrary (Arbitrary (..))

instance Arbitrary Mig where
  arbitrary = Migration <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary MockState where
  arbitrary = MockState <$> arbitrary <*> arbitrary

instance Arbitrary Action where
  arbitrary = do
    x <- choose (0 :: Int, 8)
    case x of
      0 -> return ActionEnsureStack
      1 -> return ActionGetMigrations
      2 -> return ActionBeginTransaction
      3 -> return ActionRollbackTransaction
      4 -> return ActionCommitTransaction
      5 -> do x1 <- arbitrary
              return (ActionRunMigration x1)
      6 -> do x1 <- arbitrary
              return (ActionPushMigration x1)
      7 -> return ActionPopMigration
      8 -> do x1 <- arbitrary
              return (ActionTestCondition x1)
      _ -> error "FATAL ERROR: Arbitrary instance, logic bug"

instance Arbitrary MockConnection where
  arbitrary = MockConnection <$> arbitrary <*> arbitrary <*> arbitrary
