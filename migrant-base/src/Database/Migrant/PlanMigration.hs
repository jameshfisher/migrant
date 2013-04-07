module Database.Migrant.PlanMigration
  ( Plan (..)
  , planMigration

#ifdef Testing
  , fusePrefix
  , collectUps
  , tryMigrateDown
#endif

  ) where

import Data.Maybe
import Data.Function

import Database.Migrant.Types.Migration (Migration(..))

data Plan q c
  = AbortivePlan [Migration q q c] [Migration q (Maybe q) c]
  | Plan [Migration q q c] [Migration q (Maybe q) c]

fusePrefix :: (a -> a -> Bool) -> [a] -> [a] -> ([a], [a], [a])
fusePrefix eq (a:as) (b:bs) =
  if a `eq` b
  then let (p, as', bs') = fusePrefix eq as bs
       in (a:p, as', bs')
  else ([], a:as, b:bs)
fusePrefix _ as bs = ([], as, bs)

stripPrefix :: (a -> a -> Bool) -> [a] -> [a] -> ([a], [a])
stripPrefix eq as bs = (as', bs') where (_, as', bs') = fusePrefix eq as bs

collectUps :: Eq q => [Migration q (Maybe q) c] -> [Migration q (Maybe q) c]
collectUps = filter (isNothing . migrationDown)

tryMigrateDown :: Eq q => [Migration q (Maybe q) c] -> ([Migration q q c], [Migration q (Maybe q) c])
tryMigrateDown [] = ([], [])
tryMigrateDown (Migration u md mpre mpost descn : ms) = case md of
  Nothing  ->  ([], Migration u md mpre mpost descn : collectUps ms)
  Just d   ->  let (bis, ups) = tryMigrateDown ms
              in (Migration u d mpre mpost descn : bis, ups)

planMigration :: Eq q => [Migration q (Maybe q) c] -> [Migration q (Maybe q) c] -> Plan q c
planMigration old new =
  let
    (toReverse, toPlay) = stripPrefix ((==) `on` migrationUp) old new
    (bis, ups) = tryMigrateDown (reverse toReverse)
  in case ups of 
    [] -> Plan bis toPlay
    _  -> AbortivePlan bis ups
