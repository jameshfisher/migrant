module Database.PostgreSQL.Migrate.PlanMigration
  ( Plan (..)
  , planMigration
  ) where

import Data.Function

import Database.PostgreSQL.Migrate.Data

data Plan
  = AbortivePlan [BiMigration String] [UpMigration String]
  | Plan [BiMigration String] [Migration String]

stripPrefix :: (a -> a -> Bool) -> [a] -> [a] -> ([a], [a])
stripPrefix eq (a:as) (b:bs) =
  if a `eq` b
  then stripPrefix eq as bs
  else (a:as, b:bs)
stripPrefix _ as bs = (as, bs)

collectUps :: [Migration String] -> [UpMigration String]
collectUps [] = []
collectUps (Migration n u d : ms) = case d of
  Nothing -> UpMigration n u : collectUps ms
  Just _  ->                   collectUps ms

tryMigrateDown :: [Migration String] -> ([BiMigration String], [UpMigration String])
tryMigrateDown [] = ([], [])
tryMigrateDown (Migration n u md : ms) = case md of
  Nothing  ->  ([], UpMigration n u : collectUps ms)
  Just d   ->  let (bis, ups) = tryMigrateDown ms
              in (BiMigration n u d : bis, ups)

planMigration :: [Migration String] -> [Migration String] -> Plan
planMigration old new =
  let
    (toReverse, toPlay) = stripPrefix ((==) `on` migrationUp) old new
    (bis, ups) = tryMigrateDown (reverse toReverse)
  in case ups of 
    [] -> Plan bis toPlay
    _  -> AbortivePlan bis ups
