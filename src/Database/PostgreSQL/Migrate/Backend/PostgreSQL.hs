module Database.PostgreSQL.Migrate.Backend.PostgreSQL where

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
        , up
            text
            constraint _migration_up_not_null not null
        , down
            text
        , description
            text
        );
    |]

  backendGetMigrations conn = query_ conn
    [sql|
      select up, down, description
      from _migration
      order by id asc
    |]

  backendDownMigrate conn mig = withTransaction conn $ do
    _ <- execute_ conn (Query $ fromString $ biMigrationDown mig)
    affected <- execute conn
      [sql|
        delete from _migration
        where
          id = (select max(id) from _migration)
          and up = ?
      |] (Only $ biMigrationUp mig)
    when (affected != 1) $ error "tried to down-migrate a migration that isn't the top of the stack"
    return ()

  backendUpMigrate conn mig = withTransaction conn $ do
    _ <- execute_ conn (Query $ fromString $ migrationUp mig)
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
      |] (migrationUp mig, migrationDown mig, migrationDescription mig)
    return ()
