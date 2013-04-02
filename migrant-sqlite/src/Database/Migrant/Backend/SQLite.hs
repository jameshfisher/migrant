{-# LANGUAGE QuasiQuotes #-}
module Database.Migrant.Backend.SQLite where

import MultiLineString
import Control.Monad
import Database.SQLite

import Database.Migrant.Types.Backend (Backend (..))

raiseLeft :: IO (Either String a) -> IO a
raiseLeft act = do
  res <- act
  case res of
    Left err -> error $ "Program error: " ++ err
    Right v  -> return v

instance Backend SQLiteHandle String String where

  backendEnsureStack handle = do
    rows <- raiseLeft $ execStatement handle
      [str|
        select name
          from sqlite_master
          where
            type='table'
            and name='_migration'
      |]
      
    case rows of
      [[[(_, Int n)]]] -> do
        when (n == 0) $ do
          res <- execStatement_ handle
            [str|
              create table _migration
                ( id integer
                    constraint _migration_id_pkey primary key
                    constraint _migration_id_positive check (0 < id)
                , parent integer
                    constraint _migration_id_sequence check
                      ( (id = 1 and parent is null)
                        or id = parent+1 )
                , up text
                    constraint _migration_up_not_null not null
                , down text
                , description text
                , constraint _migration_parent_fkey
                    foreign key (parent) references _migration (id)
                )
            |]
          case res of
            Nothing  -> return ()
            Just err -> error $ "Program error: " ++ err

        return (n == 0)

      _                -> error $ "Program error: " ++ show rows

  backendGetMigrations handle = do
    --rows <- raiseLeft $ execStatement handle
    --  [str|
    --    select up, down, description
    --    from _migration
    --    order by id asc
    --  |]
    return []
    -- TODO

  backendBeginTransaction    conn = return ()
  backendRollbackTransaction conn = return ()
  backendCommitTransaction   conn = return Nothing

  backendRunMigration = error "not implemented"
  backendPushMigration = error "not implemented"
  backendPopMigration = error "not implemented"

  backendTestCondition conn cond = error "not implemented"
