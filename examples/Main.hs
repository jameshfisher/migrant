{-# LANGUAGE OverloadedStrings #-}
module Main where

import Database.PostgreSQL.Simple
import Database.Migrant
import Database.Migrant.Backend.PostgreSQL (addColumn)
import Database.Migrant.Frontend.Terminal (frontendTerminal)

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
    migrateSettingsFrontend = frontendTerminal
    }

  runMigrations settings
    [ Migration "create table foo (bar integer)"   (Just "drop table foo")              Nothing Nothing (Just "first")
    , Migration "insert into foo (bar) values (1)" (Just "delete from foo where bar=1") Nothing Nothing Nothing
    , Migration
        "alter table foo add column q integer"
        (Just "alter table foo drop column q")
        (Just "select count(*) = 0 from information_schema.columns where table_name ='foo' and column_name = 'q'")
        (Just "select count(*) = 1 from information_schema.columns where table_name ='foo' and column_name = 'q'")
        (Just "add column foo.q")
    , addColumn "public" "foo" "qux" "text"
    ]

  return ()
