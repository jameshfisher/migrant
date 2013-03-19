module Database.PostgreSQL.Migrate.Main where

import Database.PostgreSQL.Simple

import Database.PostgreSQL.Migrate

main :: IO ()
main = do

  conn <- connect ConnectInfo
    { connectHost = "127.0.0.1"
    , connectPort = 5432
    , connectUser = "migrate"
    , connectPassword = "migrate"
    , connectDatabase = "migrate"
    }
  
  let settings = MigrateSettings {
    migrateSettingsBackend = conn,
    migrateSettingsInteractive = True
    }

  runMigrations settings
    [ Migration "create table foo (bar integer)"   (Just "drop table foo")              (Just "first")
    , Migration "insert into foo (bar) values (1)" (Just "delete from foo where bar=1") Nothing
    ]
