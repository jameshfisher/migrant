module Database.Migrant.Test.PlanMigration (testGroupPlanMigration) where

import Data.Maybe (isNothing, isJust)

import Test.Framework (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Test.QuickCheck (elements, NonEmptyList (..))
import Test.QuickCheck.Arbitrary (arbitrary)

import Database.Migrant.Types.Migration (Migration (migrationDown), fromBiMigration)
import Database.Migrant.Types.Mock (Mig)

import Database.Migrant.Test.Types ()
import Database.Migrant.PlanMigration (fusePrefix, collectUps, tryMigrateDown)

subseq :: Eq a => [a] -> [a] -> Bool
[]     `subseq` _      = True
(_:_ ) `subseq` []     = False
(a:as) `subseq` (b:bs) = (if a == b then as else a:as) `subseq` bs

testGroupPlanMigration :: [Test]
testGroupPlanMigration =
  [ testGroup "fusePrefix"
    [ testProperty "takes a prefix" $
        \(as :: [Int]) (bs :: [Int]) ->
          let (p, as', bs') = fusePrefix (==) as bs
          in   p ++ as' == as
            && p ++ bs' == bs

    , testProperty "takes the maximal prefix" $
        \(as :: [Int]) (bs :: [Int]) ->
          let (_, as', bs') = fusePrefix (==) as bs
          in case (as', bs') of
            (a:_, b:_) -> a /= b
            _ -> True
    ]

  , testGroup "collectUps"
    [ testProperty "returns subsequence" $
        \(as :: [Mig]) ->
          collectUps as `subseq` as

    , testProperty "only returns ups" $
        \(as :: [Mig]) -> all (isNothing . migrationDown) $ collectUps as

    , testProperty "returns all ups" $ do
        NonEmpty (as :: [Mig]) <- arbitrary
        a <- elements as
        return $ if (isNothing $ migrationDown a)
          then a `elem` collectUps as
          else True
    ]

  , testGroup "tryMigrateDown"
    [ testProperty "returns prefix of migrations" $
        \(as :: [Mig]) ->
          let (bis, _) = tryMigrateDown as
          in map fromBiMigration bis == takeWhile (isJust . migrationDown) as

    , testProperty "returns collectUps" $
        \(as :: [Mig]) -> (snd $ tryMigrateDown as) == collectUps as
    ]
  ]
