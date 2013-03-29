module Database.Migrant.PlanMigration
  ( Plan (..)
  , planMigration
  ) where

import Data.Maybe
import Data.Function

import Database.Migrant.Data

data Plan q
  = AbortivePlan [Migration q q] [Migration q (Maybe q)]
  | Plan [Migration q q] [Migration q (Maybe q)]

stripPrefix :: (a -> a -> Bool) -> [a] -> [a] -> ([a], [a])
stripPrefix eq (a:as) (b:bs) =
  if a `eq` b
  then stripPrefix eq as bs
  else (a:as, b:bs)
stripPrefix _ as bs = (as, bs)

collectUps :: Eq q => [Migration q (Maybe q)] -> [Migration q (Maybe q)]
collectUps = filter (isNothing . migrationDown)

tryMigrateDown :: Eq q => [Migration q (Maybe q)] -> ([Migration q q], [Migration q (Maybe q)])
tryMigrateDown [] = ([], [])
tryMigrateDown (Migration u md descn : ms) = case md of
  Nothing  ->  ([], Migration u md descn : collectUps ms)
  Just d   ->  let (bis, ups) = tryMigrateDown ms
              in (Migration u d descn : bis, ups)

planMigration :: Eq q => [Migration q (Maybe q)] -> [Migration q (Maybe q)] -> Plan q
planMigration old new =
  let
    (toReverse, toPlay) = stripPrefix ((==) `on` migrationUp) old new
    (bis, ups) = tryMigrateDown (reverse toReverse)
  in case ups of 
    [] -> Plan bis toPlay
    _  -> AbortivePlan bis ups
