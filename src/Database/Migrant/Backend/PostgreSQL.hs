module Database.Migrant.Backend.PostgreSQL where

import Control.Applicative
import Control.Monad
import Control.Exception
import Data.ByteString.Char8
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Types
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.SqlQQ

import Database.Migrant.Data

newtype PostgreSqlError = PostgreSqlError SqlError

instance Show PostgreSqlError where
  show (PostgreSqlError e) = unpack $ sqlErrorMsg e

catchSqlErrorEither :: IO a -> IO (Either SqlError a)
catchSqlErrorEither act = do
  ex <- try act
  case ex of
    Right r -> return $ Right r
    Left ex -> case fromException ex of
      Just ex@(SqlError{..}) -> return $ Left ex
      Nothing                -> throwIO ex

catchSqlError :: IO () -> IO (Maybe PostgreSqlError)
catchSqlError act = do
  e <- catchSqlErrorEither act
  return $ case e of
    Left e  -> Just $ PostgreSqlError e
    Right _ -> Nothing

instance FromRow (Migration Query) where
  fromRow = do
    up <- field
    down <- field
    description <- field
    return $ Migration (Query up) (Query <$> down) description

instance Backend Connection Query PostgreSqlError where

  backendEnsureStack conn = do
    createStack <- query_ conn
      [sql|
        select count(*) = 0
        from pg_tables t
        where t.schemaname = 'public'
          and t.tablename  = '_migration'
      |]

    when createStack $ void $ execute_ conn
      [sql|
        create table _migration
          ( id integer
              constraint _migration_id_pkey primary key
              constraint _migration_id_positive check (0 < id)
          , parent integer
              constraint _migration_parent_fkey references _migration(id)
              constraint _migration_id_sequence check
                ( (id = 1 and parent is null)
                  or id = parent+1 )
          , up text
              constraint _migration_up_not_null not null
          , down text
          , description text
          );
      |]

    return createStack


  backendGetMigrations conn = query_ conn
    [sql|
      select up, down, description
      from _migration
      order by id asc
    |]

  backendDownMigrate conn mig = catchSqlError $ withTransaction conn $ void $ do
    _ <- execute_ conn $ biMigrationDown mig
    affected <- execute conn
      [sql|
        delete from _migration
        where
          id = (select max(id) from _migration)
          and up = ?
      |] (Only $ fromQuery $ biMigrationUp mig)
    when (affected /= 1) $ error "tried to down-migrate a migration that isn't the top of the stack"

  backendUpMigrate conn mig = catchSqlError $ withTransaction conn $ do
    _ <- execute_ conn $ migrationUp mig
    _ <- execute conn
      [sql|
        insert into _migration (id, parent, up, down, description)
        values (
          (select
            case when (select max(id) from _migration) is null
            then 1
            else (select max(id) from _migration)+1
            end),
          (select max(id) from _migration), ?, ?, ?)
      |] (fromQuery $ migrationUp mig, fromQuery <$> migrationDown mig, migrationDescription mig)
    return ()
