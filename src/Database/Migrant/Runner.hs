module Database.Migrant.Runner
  ( runMigrations
  ) where

import Data.Maybe
import Control.Applicative ((<$>))
import Control.Monad
import Control.Monad.Reader

import Database.Migrant.Data
import Database.Migrant.PlanMigration
import Database.Migrant.Terminal

tick :: IO ()
tick = putSuccess "✔"

indent :: String -> String
indent = unlines . map ("    "++) . lines

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

migrateUI :: Backend b q e => (b -> m -> IO (Maybe e)) -> String -> (m -> String) -> m -> Runner b q e (Maybe e)
migrateUI run s showMig m = do
  bk <- migrateSettingsBackend <$> ask
  interactiveIO $ putStr $ s ++ showMig m ++ " ... "
  err <- liftIO $ run bk m
  case err of
    Nothing  -> interactiveIO tick
    Just err -> interactiveIO $ do
      putError "✘ (rolled back)"
      putError $ indent $ show err
  return err

downMigrateUI :: Backend b q e => BiMigration q -> Runner b q e (Maybe e)
downMigrateUI = migrateUI backendDownMigrate "Migrating down: " showDownMigration

upMigrateUI :: Backend b q e => Migration q -> Runner b q e (Maybe e)
upMigrateUI = migrateUI backendUpMigrate "Migrating up: " showUpMigration

-- TODO downMigrateListUI and upMigrateListUI are almost the same
downMigrateListUI :: Backend b q e => [BiMigration q] -> Runner b q e (Maybe e)
downMigrateListUI ms = case ms of
  []   -> return Nothing
  m:ms -> do
    err <- downMigrateUI m
    case err of
      Just err -> do
        interactiveIO $ putError "Down-migration halted."
        return $ Just err
      Nothing  -> downMigrateListUI ms

upMigrateListUI :: Backend b q e => [Migration q] -> Runner b q e (Maybe e)
upMigrateListUI ms = case ms of
  []   -> return Nothing
  m:ms -> do
    err <- upMigrateUI m
    case err of
      Just err -> do
        interactiveIO $ putError "Up-migration halted."
        return $ Just err
      Nothing  -> upMigrateListUI ms

-- TODO better return type
runPlan :: Backend b q e => Plan q -> Runner b q e Bool
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
        if ans == "Y"
          then do
            liftIO $ putStrLn "Down-migrating."
            err <- downMigrateListUI downs
            return $ isNothing err
          else return False
      else
        return False

  Plan downs ups -> do
    err <- downMigrateListUI downs
    case err of
      Just _  -> return False
      Nothing -> do
        err <- upMigrateListUI ups
        case err of
          Just _ -> return False
          Nothing -> do
            interactiveIO $ putSuccess $ "Done (" ++ show (length ups + length downs) ++ " actions performed)."
            return True

runMigrations' :: Backend b q e => [Migration q] -> Runner b q e Bool
runMigrations' migs = do
  bk <- migrateSettingsBackend <$> ask
  ensureTableUI
  olds <- liftIO $ backendGetMigrations bk
  runPlan $ planMigration olds migs

runMigrations :: Backend b q e => MigrateSettings b q e -> [Migration q] -> IO Bool
runMigrations settings migs = runReaderT (runMigrations' migs) settings
