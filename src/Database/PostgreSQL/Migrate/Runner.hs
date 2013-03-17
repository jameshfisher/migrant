module Database.PostgreSQL.Migrate.Runner
  ( runMigrations
  ) where

import Control.Applicative ((<$>))
import Control.Monad
import Control.Monad.Reader

import Database.PostgreSQL.Migrate.Data
import Database.PostgreSQL.Migrate.PlanMigration
import Database.PostgreSQL.Migrate.Terminal
import Database.PostgreSQL.Migrate.Api.PostgreSQL

tick :: IO ()
tick = putSuccess "✔"

type Runner = ReaderT MigrateSettings IO

ensureTableUI :: Runner ()
ensureTableUI = do
  conn <- migrateSettingsConnection <$> ask
  exists <- liftIO $ stackExists conn
  when (not exists) $ liftIO $ do
    putStr $ "Creating migration stack ... "
    createStack conn
    tick

downMigrateUI :: BiMigration -> Runner ()
downMigrateUI m = do
  conn <- migrateSettingsConnection <$> ask
  liftIO $ do
    putStr $ "Migrating down from '" ++ biMigrationName m ++ "' ... "
    downMigrate conn m
    tick

upMigrateUI :: Migration -> Runner ()
upMigrateUI m = do
  conn <- migrateSettingsConnection <$> ask
  liftIO $ do
    putStr $ "Migrating up to '" ++ migrationName m ++ "' ... "
    upMigrate conn m
    tick

runPlan :: Plan -> Runner ()
runPlan plan = case plan of
  AbortivePlan downs failed -> do
    ans <- liftIO $ do
      putError "The following down-migrations are not provided:"
      mapM_ (putStrLn . upMigrationName)  failed
      putStrLn "The following down-migrations can be performed:"
      mapM_ (putStrLn . biMigrationName)  downs
      putStrLn "Would you like to do this? [Y/n]"
      getLine
    when (ans == "Y") $ do
      liftIO $ putStrLn "Down-migrating."
      mapM_ downMigrateUI downs

  Plan downs ups -> do
    mapM_ downMigrateUI downs
    mapM_ upMigrateUI ups
    liftIO $ putSuccess $ "Done (" ++ show (length ups + length downs) ++ " actions performed)."

runMigrations' :: [Migration] -> Runner ()
runMigrations' migs = do
  conn <- migrateSettingsConnection <$> ask
  ensureTableUI
  olds <- liftIO $ getMigrations conn
  runPlan $ planMigration olds migs

runMigrations :: MigrateSettings -> [Migration] -> IO ()
runMigrations settings migs = runReaderT (runMigrations' migs) settings
