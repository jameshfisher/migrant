module Database.Migrant.Runner
  ( runMigrations
  ) where

import Data.Maybe
import Control.Applicative ((<$>))
import Control.Monad
import Control.Monad.Reader

import Database.Migrant.Data
import Database.Migrant.PlanMigration

type Runner b q e = ReaderT (MigrateSettings b q e) IO

showUpMigration :: Show q => Migration q -> String
showUpMigration m = fromMaybe (show $ migrationUp m) $ migrationDescription m

showDownMigration :: Show q => BiMigration q -> String
showDownMigration m = fromMaybe (show $ biMigrationDown m) $ biMigrationDescription m

msg :: Backend b q e => Message -> Runner b q e ()
msg m = do
  f <- migrateSettingsFrontend <$> ask
  liftIO $ f m

ensureTableUI :: Backend b q e => Runner b q e ()
ensureTableUI = do
  bk <- migrateSettingsBackend <$> ask
  created <- liftIO $ backendEnsureStack bk
  when created $ msg MessageCreatedMigrationStack

downMigrateUI :: Backend b q e => BiMigration q -> Runner b q e (Maybe e)
downMigrateUI m = do
  bk <- migrateSettingsBackend <$> ask
  liftIO $ backendBeginTransaction bk
  err <- liftIO $ backendDownMigrate bk m
  case err of
    Nothing  -> do
      liftIO $ backendCommitTransaction bk
      msg MessageMigrationCommitted
    Just err -> do
      liftIO $ backendRollbackTransaction bk
      msg . MessageMigrationRolledBack . show $ err
  return err

upMigrateUI :: Backend b q e => Migration q -> Runner b q e (Maybe e)
upMigrateUI m = do
  bk <- migrateSettingsBackend <$> ask
  liftIO $ backendBeginTransaction bk
  err <- liftIO $ backendUpMigrate bk m
  case err of
    Nothing -> do
      liftIO $ backendCommitTransaction bk
      msg MessageMigrationCommitted
    Just err -> do
      liftIO $ backendRollbackTransaction bk
      msg . MessageMigrationRolledBack . show $ err
  return err

-- always rolls back
testUpMigration :: Backend b q e => Migration q -> Runner b q e (Maybe e)
testUpMigration m@(Migration up down desc) = do
  bk <- migrateSettingsBackend <$> ask
  case down of
    Nothing -> do
      msg MessageWarnNoDownMigration
      return Nothing
    Just down -> do
      msg MessageTestingMigration
      liftIO $ backendBeginTransaction bk
      err <- liftIO $ backendUpMigrate bk m
      case err of
        Just err -> do
          liftIO $ backendRollbackTransaction bk
          msg . MessageMigrationRolledBack . show $ err
          return $ Just err
        Nothing -> do
          err <- liftIO $ backendDownMigrate bk $ BiMigration up down desc
          case err of
            Just err -> do
              liftIO $ backendRollbackTransaction bk
              msg . MessageMigrationRolledBack . show $ err
              return $ Just err
            Nothing -> do
              liftIO $ backendRollbackTransaction bk
              return Nothing

-- TODO downMigrateListUI and upMigrateListUI are almost the same
downMigrateListUI :: Backend b q e => [BiMigration q] -> Runner b q e (Maybe e)
downMigrateListUI ms = case ms of
  []   -> return Nothing
  m:ms -> do
    msg . MessageMigrationStartedDown . showDownMigration $ m
    err <- downMigrateUI m
    case err of
      Just err -> return $ Just err
      Nothing  -> downMigrateListUI ms

upMigrateListUI :: Backend b q e => [Migration q] -> Runner b q e (Maybe e)
upMigrateListUI ms = case ms of
  []   -> return Nothing
  m:ms -> do
    msg . MessageMigrationStartedUp . showUpMigration $ m
    err <- testUpMigration m
    case err of
      Just err -> return $ Just err
      Nothing  -> do
        err <- upMigrateUI m
        case err of
          Just err -> return $ Just err
          Nothing  -> do
            upMigrateListUI ms

-- TODO better return type
runPlan :: Backend b q e => Plan q -> Runner b q e Bool
runPlan plan = case plan of
  AbortivePlan _ failed -> do
    msg . MessageMissingDownMigrations . map showUpMigration $ failed
    return False

  Plan downs ups -> do
    err <- downMigrateListUI downs
    case err of
      Just _  -> return False
      Nothing -> do
        err <- upMigrateListUI ups
        case err of
          Just _ -> do
            msg MessageAborted
            return False
          Nothing -> do
            msg . MessageCompleted $ length ups + length downs
            return True

runMigrations' :: Backend b q e => [Migration q] -> Runner b q e Bool
runMigrations' migs = do
  bk <- migrateSettingsBackend <$> ask
  ensureTableUI
  olds <- liftIO $ backendGetMigrations bk
  runPlan $ planMigration olds migs

runMigrations :: Backend b q e => MigrateSettings b q e -> [Migration q] -> IO Bool
runMigrations settings migs = runReaderT (runMigrations' migs) settings
