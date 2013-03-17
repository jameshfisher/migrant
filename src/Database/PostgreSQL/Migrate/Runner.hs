module Database.PostgreSQL.Migrate.Runner
  ( runMigrations
  ) where

import Control.Monad
import Database.PostgreSQL.Simple

import Database.PostgreSQL.Migrate.Data
import Database.PostgreSQL.Migrate.PlanMigration
import Database.PostgreSQL.Migrate.Terminal
import Database.PostgreSQL.Migrate.Api.PostgreSQL

tick :: IO ()
tick = putSuccess "✔"

ensureTableUI :: Connection -> IO ()
ensureTableUI conn = do
  created <- ensureTable conn
  when created tick

downMigrateUI :: Connection -> BiMigration -> IO ()
downMigrateUI conn m = do
  putStr $ "Migrating down from '" ++ biMigrationName m ++ "' ... "
  downMigrate conn m
  tick

upMigrateUI :: Connection -> Migration -> IO ()
upMigrateUI conn m = do
  putStr $ "Migrating up to '" ++ migrationName m ++ "' ... "
  upMigrate conn m
  tick

runPlan :: Connection -> Plan -> IO ()
runPlan conn plan = case plan of
  AbortivePlan downs failed -> do
    putError "The following down-migrations are not provided:"
    mapM_ (putStrLn . upMigrationName)  failed
    putStrLn "The following down-migrations can be performed:"
    mapM_ (putStrLn . biMigrationName)  downs
    putStrLn "Would you like to do this? [Y/n]"
    ans <- getLine
    when (ans == "Y") $ do
      putStrLn "Down-migrating."
      mapM_ (downMigrateUI conn) downs

  Plan downs ups -> do
    mapM_ (downMigrateUI conn) downs
    mapM_ (upMigrateUI conn) ups
    putSuccess $ "Done (" ++ show (length ups + length downs) ++ " actions performed)."

runMigrations :: Connection -> [Migration] -> IO ()
runMigrations conn migs = do
  ensureTableUI conn
  olds <- getMigrations conn
  runPlan conn (planMigration olds migs)
