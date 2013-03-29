module Database.Migrant.Runner
  ( runMigrations
  ) where

import Data.Maybe
import Control.Applicative ((<$>))
import Control.Monad
import Control.Monad.Reader

import Database.Migrant.Data
import Database.Migrant.PlanMigration

type Runner conn q cond e = ReaderT (MigrateSettings conn q cond e) IO

showUpMigration :: Show q => Migration q (Maybe q) c -> String
showUpMigration m = fromMaybe (show $ migrationUp m) $ migrationDescription m

showDownMigration :: Show q => Migration q q c -> String
showDownMigration m = fromMaybe (show $ migrationDown m) $ migrationDescription m

msg :: Backend conn q cond e => Message -> Runner conn q cond e ()
msg m = do
  f <- migrateSettingsFrontend <$> ask
  liftIO $ f m

ensureTableUI :: Backend conn q cond e => Runner conn q cond e ()
ensureTableUI = do
  bk <- migrateSettingsBackend <$> ask
  created <- liftIO $ backendEnsureStack bk
  when created $ msg MessageCreatedMigrationStack

downMigrateUI :: Backend conn q cond e => Migration q q cond -> Runner conn q cond e (Maybe e)
downMigrateUI m = do
  bk <- migrateSettingsBackend <$> ask
  liftIO $ backendBeginTransaction bk
  err <- liftIO $ backendRunMigration bk (migrationDown m)
  case err of
    Nothing -> do
      liftIO $ backendPopMigration bk
      err <- liftIO $ backendCommitTransaction bk
      case err of
        Nothing  -> msg MessageMigrationCommitted
        Just err -> msg . MessageMigrationRolledBack . show $ err
      return err
    Just err -> do
      liftIO $ backendRollbackTransaction bk
      msg . MessageMigrationRolledBack . show $ err
      return $ Just err

upMigrateUI :: Backend conn q cond e => Migration q (Maybe q) cond -> Runner conn q cond e (Maybe e)
upMigrateUI m = do
  bk <- migrateSettingsBackend <$> ask
  liftIO $ backendBeginTransaction bk
  err <- liftIO $ backendRunMigration bk (migrationUp m)
  case err of
    Nothing -> do
      liftIO $ backendPushMigration bk m
      err <- liftIO $ backendCommitTransaction bk
      case err of
        Nothing  -> msg MessageMigrationCommitted
        Just err -> msg . MessageMigrationRolledBack . show $ err
      return err
    Just err -> do
      liftIO $ backendRollbackTransaction bk
      msg . MessageMigrationRolledBack . show $ err
      return $ Just err

-- always rolls back
testUpMigration :: Backend conn q cond e => Migration q (Maybe q) cond -> Runner conn q cond e (Maybe e)
testUpMigration (Migration up down _ _ _) = do
  bk <- migrateSettingsBackend <$> ask
  case down of
    Nothing -> do
      msg MessageWarnNoDownMigration
      return Nothing
    Just down -> do
      msg MessageTestingMigration
      liftIO $ backendBeginTransaction bk
      err <- liftIO $ backendRunMigration bk up
      case err of
        Just err -> do
          liftIO $ backendRollbackTransaction bk
          msg . MessageMigrationRolledBack . show $ err
          return $ Just err
        Nothing -> do
          err <- liftIO $ backendRunMigration bk down
          case err of
            Just err -> do
              liftIO $ backendRollbackTransaction bk
              msg . MessageMigrationRolledBack . show $ err
              return $ Just err
            Nothing -> do
              liftIO $ backendRollbackTransaction bk
              return Nothing

-- TODO downMigrateListUI and upMigrateListUI are almost the same
downMigrateListUI :: Backend conn q cond e => [Migration q q cond] -> Runner conn q cond e (Maybe e)
downMigrateListUI ms = case ms of
  []   -> return Nothing
  m:ms -> do
    msg . MessageMigrationStartedDown . showDownMigration $ m
    err <- downMigrateUI m
    case err of
      Just err -> return $ Just err
      Nothing  -> downMigrateListUI ms

upMigrateListUI :: Backend conn q cond e => [Migration q (Maybe q) cond] -> Runner conn q cond e (Maybe e)
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
runPlan :: Backend conn q cond e => Plan q cond -> Runner conn q cond e Bool
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

runMigrations' :: Backend conn q cond e => [Migration q (Maybe q) cond] -> Runner conn q cond e Bool
runMigrations' migs = do
  bk <- migrateSettingsBackend <$> ask
  ensureTableUI
  olds <- liftIO $ backendGetMigrations bk
  runPlan $ planMigration olds migs

runMigrations :: Backend conn q cond e => MigrateSettings conn q cond e -> [Migration q (Maybe q) cond] -> IO Bool
runMigrations settings migs = runReaderT (runMigrations' migs) settings
