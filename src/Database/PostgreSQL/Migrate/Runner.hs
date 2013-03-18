module Database.PostgreSQL.Migrate.Runner
  ( runMigrations
  ) where

import Data.List
import Control.Applicative ((<$>))
import Control.Monad
import Control.Monad.Reader

import Database.PostgreSQL.Migrate.Data
import Database.PostgreSQL.Migrate.PlanMigration
import Database.PostgreSQL.Migrate.Terminal

tick :: IO ()
tick = putSuccess "✔"

type Runner b q = ReaderT (MigrateSettings b q) IO

whenInteractive :: Backend b q => Runner b q () -> Runner b q ()
whenInteractive a = do
  interactive <- migrateSettingsInteractive <$> ask
  when interactive a

interactiveIO :: Backend b q => IO () -> Runner b q ()
interactiveIO = whenInteractive . liftIO

ensureTableUI :: Backend b q => Runner b q ()
ensureTableUI = do
  bk <- migrateSettingsBackend <$> ask
  exists <- liftIO $ backendStackExists bk
  unless exists $ do
    interactiveIO $ putStr "Creating migration stack ... "
    liftIO $ backendCreateStack bk
    interactiveIO tick

downMigrateUI :: Backend b q => BiMigration q -> Runner b q ()
downMigrateUI m = do
  bk <- migrateSettingsBackend <$> ask
  interactiveIO $ putStr $ "Migrating down from '" ++ biMigrationName m ++ "' ... "
  liftIO $ backendDownMigrate bk m
  interactiveIO tick

upMigrateUI :: Backend b q => Migration q -> Runner b q ()
upMigrateUI m = do
  bk <- migrateSettingsBackend <$> ask
  interactiveIO $ putStr $ "Migrating up to '" ++ migrationName m ++ "' ... "
  liftIO $ backendUpMigrate bk m
  interactiveIO tick

runPlan :: Backend b q => Plan q -> Runner b q ()
runPlan plan = case plan of
  AbortivePlan downs failed -> do
    interactive <- migrateSettingsInteractive <$> ask
    if interactive
      then do
        ans <- liftIO $ do
          putError "The following down-migrations are not provided:"
          mapM_ (putStrLn . migrationName)  failed
          putStrLn "The following down-migrations can be performed:"
          mapM_ (putStrLn . biMigrationName)  downs
          putStrLn "Would you like to do this? [Y/n]"
          getLine
        when (ans == "Y") $ do
          liftIO $ putStrLn "Down-migrating."
          mapM_ downMigrateUI downs
      else 
        error $ "The following down-migrations are not provided: " ++ intercalate ", " (map migrationName failed)

  Plan downs ups -> do
    mapM_ downMigrateUI downs
    mapM_ upMigrateUI ups
    interactiveIO $ putSuccess $ "Done (" ++ show (length ups + length downs) ++ " actions performed)."

runMigrations' :: Backend b q => [Migration q] -> Runner b q ()
runMigrations' migs = do
  bk <- migrateSettingsBackend <$> ask
  ensureTableUI
  olds <- liftIO $ backendGetMigrations bk
  runPlan $ planMigration olds migs

runMigrations :: Backend b q => MigrateSettings b q -> [Migration q] -> IO ()
runMigrations settings migs = runReaderT (runMigrations' migs) settings
