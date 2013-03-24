{-# LANGUAGE QuasiQuotes #-}
module Database.Migrant.Backend.SQLite where

import MultiLineString
import Database.SQLite

import Database.Migrant.Data

raiseLeft :: IO (Either String a) -> IO a
raiseLeft act = do
  res <- act
  case res of
    Left err -> error $ "Program error: " ++ err
    Right v  -> return v

instance Backend SQLiteHandle String String where
  backendStackExists handle = do
    rows <- raiseLeft $ execStatement handle
      [str|
        select name
          from sqlite_master
          where
            type='table'
            and name='_migration'
      |]
    case rows of
      [[[(_, Int n)]]] -> return $ n == 1
      _                -> error $ "Program error: " ++ show rows

  backendCreateStack handle = do
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

  backendGetMigrations handle = do
    --rows <- raiseLeft $ execStatement handle
    --  [str|
    --    select up, down, description
    --    from _migration
    --    order by id asc
    --  |]
    return []
    -- TODO

  backendDownMigrate handle mig = return (Just "not implemented")
  backendUpMigrate handle mig = return (Just "not implemented")
