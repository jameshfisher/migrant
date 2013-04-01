module Database.Migrant.Backend.PostgreSQL
  ( addColumn
  ) where

import Control.Applicative
import Control.Monad
import Control.Exception
import Data.ByteString.Char8
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Types
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.SqlQQ

import Database.Migrant.Types.Migration (Migration (..))
import Database.Migrant.Types.Backend (Backend (..))

catchSqlErrorEither :: IO a -> IO (Either SqlError a)
catchSqlErrorEither act = do
  ex <- try act
  case ex of
    Right r -> return $ Right r
    Left ex -> case fromException ex of
      Just ex@(SqlError{..}) -> return $ Left ex
      Nothing                -> throwIO ex

catchSqlError :: IO () -> IO (Maybe String)
catchSqlError act = do
  e <- catchSqlErrorEither act
  return $ case e of
    Left e  -> Just . unpack . sqlErrorMsg $ e
    Right _ -> Nothing

instance FromRow (Migration Query (Maybe Query) Query) where
  fromRow = do
    up <- field
    down <- field
    pre <- field
    post <- field
    description <- field
    return $ Migration (Query up) (Query <$> down) (Query <$> pre) (Query <$> post) description

instance Backend Connection Query Query where

  backendEnsureStack conn = do
    [[createStack]] <- query_ conn
      [sql|
        select count(*) = 0
          from pg_namespace
          where nspname = 'migrant'
      |]

    when createStack $ void $ execute_ conn
      [sql|
        set client_min_messages='warning'; /* ignore annoying 'implicit index' message */

        create schema migrant
          create table migrant.migration
            ( id integer
                constraint migration_id_pkey primary key
                constraint migration_id_positive check (0 < id)
            , parent integer
                constraint migration_parent_fkey references migrant.migration(id)
                constraint migration_id_sequence check
                  ( (id = 1 and parent is null)
                    or id = parent+1 )
            , up text
                constraint migration_up_not_null not null
            , down text
            , pre text
            , post text
            , description text
            );
      |]

    return createStack

  backendGetMigrations conn = query_ conn
    [sql|
      select up, down, pre, post, description
      from migrant.migration
      order by id asc
    |]

  backendBeginTransaction conn = do
    begin conn
    void $ execute_ conn "lock table migrant.migration in exclusive mode"

  backendRollbackTransaction = rollback
  backendCommitTransaction = catchSqlError . commit

  backendRunMigration conn = catchSqlError . void . execute_ conn

  backendPushMigration conn mig = void $ execute conn
    [sql|
      insert into migrant.migration (id, parent, up, down, pre, post, description)
      values (
        (select
          case when (select max(id) from migrant.migration) is null
          then 1
          else (select max(id) from migrant.migration)+1
          end),
        (select max(id) from migrant.migration), ?, ?, ?, ?, ?)
    |] (
      fromQuery $ migrationUp mig,
      fromQuery <$> migrationDown mig,
      fromQuery <$> migrationPre mig,
      fromQuery <$> migrationPost mig,
      migrationDescription mig)

  backendPopMigration conn = void $ execute_ conn
    [sql|
      delete from migrant.migration
      where id = (select max(id) from migrant.migration)
    |]

  backendTestCondition conn cond = do
    [[pass]] <- query_ conn cond
    return pass

addColumn :: String -> String -> String -> String -> Migration Query (Maybe Query) Query
addColumn schema table col ty = Migration
  (Query . pack $ "alter table " ++ schema ++ "." ++ table ++ " add column " ++ col ++ " " ++ ty)
  (Just . Query . pack $ "alter table " ++ schema ++ "." ++ table ++ " drop column " ++ col)
  (Just . Query . pack $ "select count(*) = 0 from information_schema.columns where table_schema='" ++ schema ++ "' and table_name ='" ++ table ++ "' and column_name = '" ++ col ++ "'")
  (Just . Query . pack $ "select count(*) = 1 from information_schema.columns where table_schema='" ++ schema ++ "' and table_name ='" ++ table ++ "' and column_name = '" ++ col ++ "'")
  (Just $ "add column " ++ schema ++ "." ++ table ++ "." ++ col)
