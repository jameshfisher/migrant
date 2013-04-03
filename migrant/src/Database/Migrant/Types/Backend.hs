module Database.Migrant.Types.Backend
  ( Backend (..)
  ) where

import Database.Migrant.Types.Migration (Migration (..))

class (Eq q, Show q, Eq cond, Show cond, Functor m, Monad m) => Backend conn q cond m | conn -> q cond m where
  backendEnsureStack         :: conn -> m Bool
  backendGetMigrations       :: conn -> m [Migration q (Maybe q) cond]
  backendBeginTransaction    :: conn -> m ()
  backendRollbackTransaction :: conn -> m ()
  backendCommitTransaction   :: conn -> m (Maybe String)
  backendRunMigration        :: conn -> q -> m (Maybe String)
  backendPushMigration       :: conn -> Migration q (Maybe q) cond -> m ()
  backendPopMigration        :: conn -> m ()
  backendTestCondition       :: conn -> cond -> m Bool
