module Database.Migrant.Test.PlanMigration (testGroupPlanMigration) where

import Test.Framework (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Database.Migrant.Test.Types ()
import Database.Migrant.PlanMigration (fusePrefix)

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
            (a:as'', b:bs'') -> a /= b
            _ -> True
    ]
  ]
