module Database.Migrant.Types.Backend
  ( Backend (..)
  , UpMigration
  , BiMigration
  ) where

import Database.Migrant.Types.Migration (Migration (..))

class (Show (BackendQuery conn), Eq (BackendQuery conn), Eq (BackendCond conn), Show (BackendCond conn), Functor (BackendMonad conn), Monad (BackendMonad conn)) =>
    Backend conn where

  type BackendMonad conn :: * -> *
  type BackendQuery conn :: *
  type BackendCond  conn :: *

  backendEnsureStack         :: conn -> BackendMonad conn Bool
  backendGetMigrations       :: conn -> BackendMonad conn [Migration (BackendQuery conn) (Maybe (BackendQuery conn)) (BackendCond conn)]
  backendBeginTransaction    :: conn -> BackendMonad conn ()
  backendRollbackTransaction :: conn -> BackendMonad conn ()
  backendCommitTransaction   :: conn -> BackendMonad conn (Maybe String)
  backendRunQuery            :: conn -> BackendQuery conn -> BackendMonad conn (Maybe String)
  backendPushMigration       :: conn -> Migration (BackendQuery conn) (Maybe (BackendQuery conn)) (BackendCond conn) -> BackendMonad conn ()
  backendPopMigration        :: conn -> BackendMonad conn ()
  backendTestCondition       :: conn -> BackendCond conn -> BackendMonad conn (Maybe String)

type UpMigration conn = Migration (BackendQuery conn) (Maybe (BackendQuery conn)) (BackendCond conn)
type BiMigration conn = Migration (BackendQuery conn) (BackendQuery conn) (BackendCond conn)
