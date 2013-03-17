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
  
  runMigrations (MigrateSettings conn)
    [ Migration "first"   "create table foo (bar integer)"   (Just "drop table foo")
    , Migration "another" "insert into foo (bar) values (1)" (Just "delete from foo where bar=1")
    ]
