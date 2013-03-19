module Database.Migrant.PlanMigration
  ( Plan (..)
  , planMigration
  ) where

import Data.Maybe
import Data.Function

import Database.Migrant.Data

data Plan q
  = AbortivePlan [BiMigration q] [Migration q]
  | Plan [BiMigration q] [Migration q]

stripPrefix :: (a -> a -> Bool) -> [a] -> [a] -> ([a], [a])
stripPrefix eq (a:as) (b:bs) =
  if a `eq` b
  then stripPrefix eq as bs
  else (a:as, b:bs)
stripPrefix _ as bs = (as, bs)

collectUps :: Eq q => [Migration q] -> [Migration q]
collectUps = filter (isNothing . migrationDown)

tryMigrateDown :: Eq q => [Migration q] -> ([BiMigration q], [Migration q])
tryMigrateDown [] = ([], [])
tryMigrateDown (Migration u md descn : ms) = case md of
  Nothing  ->  ([], Migration u md descn : collectUps ms)
  Just d   ->  let (bis, ups) = tryMigrateDown ms
              in (BiMigration u d descn : bis, ups)

planMigration :: Eq q => [Migration q] -> [Migration q] -> Plan q
planMigration old new =
  let
    (toReverse, toPlay) = stripPrefix ((==) `on` migrationUp) old new
    (bis, ups) = tryMigrateDown (reverse toReverse)
  in case ups of 
    [] -> Plan bis toPlay
    _  -> AbortivePlan bis ups
