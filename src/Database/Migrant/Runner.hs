module Database.Migrant.Runner
  ( runMigrations
  ) where

import Data.Maybe
import Data.List
import Control.Applicative ((<$>))
import Control.Monad
import Control.Monad.Reader

import Database.Migrant.Data
import Database.Migrant.PlanMigration
import Database.Migrant.Terminal

tick :: IO ()
tick = putSuccess "✔"

showUpMigration :: Show q => Migration q -> String
showUpMigration m = fromMaybe (show $ migrationUp m) $ migrationDescription m

showDownMigration :: Show q => BiMigration q -> String
showDownMigration m = fromMaybe (show $ biMigrationDown m) $ biMigrationDescription m

type Runner b q e = ReaderT (MigrateSettings b q e) IO

whenInteractive :: Backend b q e => Runner b q e () -> Runner b q e ()
whenInteractive a = do
  interactive <- migrateSettingsInteractive <$> ask
  when interactive a

interactiveIO :: Backend b q e => IO () -> Runner b q e ()
interactiveIO = whenInteractive . liftIO

ensureTableUI :: Backend b q e => Runner b q e ()
ensureTableUI = do
  bk <- migrateSettingsBackend <$> ask
  exists <- liftIO $ backendStackExists bk
  unless exists $ do
    interactiveIO $ putStr "Creating migration stack ... "
    liftIO $ backendCreateStack bk
    interactiveIO tick

downMigrateUI :: Backend b q e => BiMigration q -> Runner b q e ()
downMigrateUI m = do
  bk <- migrateSettingsBackend <$> ask
  interactiveIO $ putStr $ "Migrating down: " ++ showDownMigration m ++ " ... "
  liftIO $ backendDownMigrate bk m
  interactiveIO tick

upMigrateUI :: Backend b q e => Migration q -> Runner b q e ()
upMigrateUI m = do
  bk <- migrateSettingsBackend <$> ask
  interactiveIO $ putStr $ "Migrating up: " ++ showUpMigration m ++ " ... "
  liftIO $ backendUpMigrate bk m
  interactiveIO tick

runPlan :: Backend b q e => Plan q -> Runner b q e ()
runPlan plan = case plan of
  AbortivePlan downs failed -> do
    interactive <- migrateSettingsInteractive <$> ask
    if interactive
      then do
        ans <- liftIO $ do
          putError "The following down-migrations are not provided:"
          mapM_ (putStrLn . showUpMigration)  failed
          putStrLn "The following down-migrations can be performed:"
          mapM_ (putStrLn . showDownMigration)  downs
          putStrLn "Would you like to do this? [Y/n]"
          getLine
        when (ans == "Y") $ do
          liftIO $ putStrLn "Down-migrating."
          mapM_ downMigrateUI downs
      else 
        error $ "The following down-migrations are not provided: " ++ intercalate ", " (map showUpMigration failed)

  Plan downs ups -> do
    mapM_ downMigrateUI downs
    mapM_ upMigrateUI ups
    interactiveIO $ putSuccess $ "Done (" ++ show (length ups + length downs) ++ " actions performed)."

runMigrations' :: Backend b q e => [Migration q] -> Runner b q e ()
runMigrations' migs = do
  bk <- migrateSettingsBackend <$> ask
  ensureTableUI
  olds <- liftIO $ backendGetMigrations bk
  runPlan $ planMigration olds migs

runMigrations :: Backend b q e => MigrateSettings b q e -> [Migration q] -> IO ()
runMigrations settings migs = runReaderT (runMigrations' migs) settings
