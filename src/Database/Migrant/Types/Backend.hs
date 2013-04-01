module Database.Migrant.Types.Backend
  ( Backend (..)
  ) where

import Database.Migrant.Types.Migration (Migration (..))

class (Eq q, Show q, Eq cond, Show cond) => Backend conn q cond | conn -> q cond where
  backendEnsureStack         :: conn -> IO Bool
  backendGetMigrations       :: conn -> IO [Migration q (Maybe q) cond]
  backendBeginTransaction    :: conn -> IO ()
  backendRollbackTransaction :: conn -> IO ()
  backendCommitTransaction   :: conn -> IO (Maybe String)
  backendRunMigration        :: conn -> q -> IO (Maybe String)
  backendPushMigration       :: conn -> Migration q (Maybe q) cond -> IO ()
  backendPopMigration        :: conn -> IO ()
  backendTestCondition       :: conn -> cond -> IO Bool
