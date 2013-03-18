module Database.PostgreSQL.Migrate.Api.PostgreSQL where

import Control.Applicative
import Control.Monad
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Types
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.SqlQQ
import Data.String (IsString(fromString))

import Database.PostgreSQL.Migrate.Data

data DbInt = DbInt Int
instance FromRow DbInt where
  fromRow = DbInt <$> field

instance FromRow (Migration String) where
  fromRow = Migration <$> field <*> field <*> field

instance Backend Connection String where
  backendStackExists conn = do
    [DbInt count] <- query_ conn
      [sql|
        select count(*)
        from pg_tables t
        where t.schemaname = 'public'
          and t.tablename  = '_migration'
      |]
    return $ count == 1

  backendCreateStack conn = void $ execute_ conn
    [sql|
      create table _migration
        ( id
            integer
            constraint _migration_id_pkey primary key
            constraint _migration_id_positive check (0 < id)
        , parent
            integer
            constraint _migration_parent_fkey references _migration(id)
            constraint _migration_id_sequence check
              ( (id = 1 and parent is null)
                or id = parent+1 )
        , name
            text
            constraint _migration_name_not_null not null
            constraint _migration_name_unique unique
        , up
            text
            constraint _migration_up_not_null not null
        , down
            text
        );
    |]

  backendGetMigrations conn = query_ conn
    [sql|
      select name, up, down
      from _migration
      order by id asc
    |]

  backendDownMigrate conn mig = withTransaction conn $ do
    _ <- execute_ conn (Query $ fromString $ biMigrationDown mig)
    _ <- execute conn
      [sql|
        delete from _migration
        where name = ?
      |] (Only $ biMigrationName mig)
    return ()

  backendUpMigrate conn mig = withTransaction conn $ do
    _ <- execute_ conn (Query $ fromString $ migrationUp mig)
    _ <- execute conn
      [sql|
        insert into _migration (id, parent, name, up, down)
        values (
          (select
            case when (select max(id) from _migration) is null
            then 1
            else (select max(id) from _migration)+1
            end),
          (select max(id) from _migration), ?, ?, ?)
      |] (migrationName mig, migrationUp mig, migrationDown mig)
    return ()
