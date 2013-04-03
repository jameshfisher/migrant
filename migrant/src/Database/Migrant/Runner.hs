module Database.Migrant.Runner
  ( runMigrations
  ) where

import Data.Maybe
import Control.Applicative ((<$>))
import Control.Monad
import Control.Monad.Reader

import Database.Migrant.Types.Migration (Migration (..))
import Database.Migrant.Types.Backend (Backend (..))
import Database.Migrant.Types.Message (Message (..))
import Database.Migrant.Types.MigrateSettings (MigrateSettings (..))

import Database.Migrant.PlanMigration

type Runner conn query cond m = ReaderT (MigrateSettings conn query cond m) m

showUpMigration :: Show query => Migration query (Maybe query) cond -> String
showUpMigration m = fromMaybe (show $ migrationUp m) $ migrationDescription m

showDownMigration :: Show query => Migration query query cond -> String
showDownMigration m = fromMaybe (show $ migrationDown m) $ migrationDescription m

msg :: Backend conn query cond m => Message -> Runner conn query cond m ()
msg m = do
  f <- migrateSettingsFrontend <$> ask
  lift $ f m

ensureTableUI :: Backend conn query cond m => Runner conn query cond m ()
ensureTableUI = do
  conn <- migrateSettingsBackend <$> ask
  created <- lift $ backendEnsureStack conn
  when created $ msg MessageCreatedMigrationStack

testMaybeCondition ::
  Backend conn query cond m =>
  Message ->
  Message ->
  Maybe cond ->
  Runner conn query cond m (Maybe String) ->
  Runner conn query cond m (Maybe String)
testMaybeCondition ifAbsent ifPresent cond next = case cond of
  Nothing -> do
    msg ifAbsent
    next
  Just cond -> do
    msg ifPresent
    conn <- migrateSettingsBackend <$> ask
    pass <- lift $ backendTestCondition conn cond
    if pass
      then next
      else do
        lift $ backendRollbackTransaction conn
        return $ Just "condition failed"

testPrecondition :: Backend conn query cond m => Maybe cond -> Runner conn query cond m (Maybe String) -> Runner conn query cond m (Maybe String)
testPrecondition  = testMaybeCondition MessageWarnNoPrecondition  MessageTestingPrecondition

testPostcondition :: Backend conn query cond m => Maybe cond -> Runner conn query cond m (Maybe String) -> Runner conn query cond m (Maybe String)
testPostcondition = testMaybeCondition MessageWarnNoPostCondition MessageTestingPostcondition

runMigration :: Backend conn query cond m => query -> Runner conn query cond m (Maybe String) -> Runner conn query cond m (Maybe String)
runMigration query next = do
  conn <- migrateSettingsBackend <$> ask
  err <- lift $ backendRunMigration conn query
  case err of
    Just err -> do
      lift $ backendRollbackTransaction conn
      return $ Just err
    Nothing -> next

transact :: Backend conn query cond m => Runner conn query cond m (Maybe String) -> Runner conn query cond m (Maybe String)
transact action = do
  conn <- migrateSettingsBackend <$> ask
  lift $ backendBeginTransaction conn
  err <- action
  case err of
    Just err -> do
      msg . MessageMigrationRolledBack $ err
      return $ Just err
    Nothing -> do
      err <- lift $ backendCommitTransaction conn
      case err of
        Nothing  -> msg MessageMigrationCommitted
        Just err -> msg . MessageMigrationRolledBack $ err
      return err

downMigrateUI :: Backend conn query cond m => Migration query query cond -> Runner conn query cond m (Maybe String)
downMigrateUI m = transact $
  testPostcondition (migrationPost m) $
    runMigration (migrationDown m) $
      testPrecondition (migrationPre m) $ do
        conn <- migrateSettingsBackend <$> ask
        lift $ backendPopMigration conn
        return Nothing

upMigrateUI :: Backend conn query cond m => Migration query (Maybe query) cond -> Runner conn query cond m (Maybe String)
upMigrateUI m = transact $
  testPrecondition (migrationPre m) $
    runMigration (migrationUp m) $
      testPostcondition (migrationPost m) $ do
        case migrationDown m of
          Nothing -> do
            msg MessageWarnNoDownMigration
            conn <- migrateSettingsBackend <$> ask
            lift $ backendPushMigration conn m
            return Nothing
          Just down -> do
            msg MessageTestingDownMigration
            runMigration down $
              testPrecondition (migrationPre m) $
                runMigration (migrationUp m) $
                  testPostcondition (migrationPost m) $ do
                    conn <- migrateSettingsBackend <$> ask
                    lift $ backendPushMigration conn m
                    return Nothing

-- TODO downMigrateListUI and upMigrateListUI are almost the same
downMigrateListUI :: Backend conn query cond m => [Migration query query cond] -> Runner conn query cond m (Maybe String)
downMigrateListUI ms = case ms of
  []   -> return Nothing
  m:ms -> do
    msg . MessageMigrationStartedDown . showDownMigration $ m
    err <- downMigrateUI m
    case err of
      Just err -> return $ Just err
      Nothing  -> downMigrateListUI ms

upMigrateListUI :: Backend conn query cond m => [Migration query (Maybe query) cond] -> Runner conn query cond m (Maybe String)
upMigrateListUI ms = case ms of
  []   -> return Nothing
  m:ms -> do
    msg . MessageMigrationStartedUp . showUpMigration $ m
    err <- upMigrateUI m
    case err of
      Just err -> return $ Just err
      Nothing  -> upMigrateListUI ms

-- TODO better return type
runPlan :: Backend conn query cond m => Plan query cond -> Runner conn query cond m Bool
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

runMigrations' :: Backend conn query cond m => [Migration query (Maybe query) cond] -> Runner conn query cond m Bool
runMigrations' migs = do
  conn <- migrateSettingsBackend <$> ask
  ensureTableUI
  olds <- lift $ backendGetMigrations conn
  runPlan $ planMigration olds migs

runMigrations :: Backend conn query cond m => MigrateSettings conn query cond m -> [Migration query (Maybe query) cond] -> m Bool
runMigrations settings migs = runReaderT (runMigrations' migs) settings
