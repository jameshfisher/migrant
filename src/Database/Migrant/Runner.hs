module Database.Migrant.Runner
  ( runMigrations
  ) where

import Data.Maybe
import Control.Applicative ((<$>))
import Control.Monad
import Control.Monad.Reader

import Database.Migrant.Data
import Database.Migrant.PlanMigration

type Runner conn query cond = ReaderT (MigrateSettings conn query cond) IO

showUpMigration :: Show query => Migration query (Maybe query) cond -> String
showUpMigration m = fromMaybe (show $ migrationUp m) $ migrationDescription m

showDownMigration :: Show query => Migration query query cond -> String
showDownMigration m = fromMaybe (show $ migrationDown m) $ migrationDescription m

msg :: Backend conn query cond => Message -> Runner conn query cond ()
msg m = do
  f <- migrateSettingsFrontend <$> ask
  liftIO $ f m

ensureTableUI :: Backend conn query cond => Runner conn query cond ()
ensureTableUI = do
  conn <- migrateSettingsBackend <$> ask
  created <- liftIO $ backendEnsureStack conn
  when created $ msg MessageCreatedMigrationStack

testMaybeCondition ::
  Backend conn query cond =>
  Message ->
  Message ->
  Maybe cond ->
  Runner conn query cond (Maybe String) ->
  Runner conn query cond (Maybe String)
testMaybeCondition ifAbsent ifPresent cond next = case cond of
  Nothing -> do
    msg ifAbsent
    next
  Just cond -> do
    msg ifPresent
    conn <- migrateSettingsBackend <$> ask
    pass <- liftIO $ backendTestCondition conn cond
    if pass
      then next
      else do
        liftIO $ backendRollbackTransaction conn
        msg . MessageMigrationRolledBack $ "precondition failed"
        return $ Just "precondition failed"

testPrecondition :: Backend conn query cond => Maybe cond -> Runner conn query cond (Maybe String) -> Runner conn query cond (Maybe String)
testPrecondition  = testMaybeCondition MessageWarnNoPrecondition  MessageTestingPrecondition

testPostcondition :: Backend conn query cond => Maybe cond -> Runner conn query cond (Maybe String) -> Runner conn query cond (Maybe String)
testPostcondition = testMaybeCondition MessageWarnNoPostCondition MessageTestingPostcondition

runMigration :: Backend conn query cond => query -> Runner conn query cond (Maybe String) -> Runner conn query cond (Maybe String)
runMigration query next = do
  conn <- migrateSettingsBackend <$> ask
  err <- liftIO $ backendRunMigration conn query
  case err of
    Just err -> do
      liftIO $ backendRollbackTransaction conn
      msg . MessageMigrationRolledBack . show $ err
      return $ Just err
    Nothing -> next

transact :: Backend conn query cond => Runner conn query cond (Maybe String) -> Runner conn query cond (Maybe String)
transact action = do
  conn <- migrateSettingsBackend <$> ask
  liftIO $ backendBeginTransaction conn
  err <- action
  case err of
    Just err -> do
      msg . MessageMigrationRolledBack . show $ err
      return $ Just err
    Nothing -> do
      err <- liftIO $ backendCommitTransaction conn
      case err of
        Nothing  -> msg MessageMigrationCommitted
        Just err -> msg . MessageMigrationRolledBack . show $ err
      return err

downMigrateUI :: Backend conn query cond => Migration query query cond -> Runner conn query cond (Maybe String)
downMigrateUI m = transact $
  testPostcondition (migrationPost m) $
    runMigration (migrationDown m) $
      testPrecondition (migrationPre m) $ do
        conn <- migrateSettingsBackend <$> ask
        liftIO $ backendPopMigration conn
        return Nothing

upMigrateUI :: Backend conn query cond => Migration query (Maybe query) cond -> Runner conn query cond (Maybe String)
upMigrateUI m = transact $
  testPrecondition (migrationPre m) $
    runMigration (migrationUp m) $
      testPostcondition (migrationPost m) $ do
        case migrationDown m of
          Nothing -> do
            msg MessageWarnNoDownMigration
            conn <- migrateSettingsBackend <$> ask
            liftIO $ backendPushMigration conn m
            return Nothing
          Just down -> do
            msg MessageTestingDownMigration
            runMigration down $
              testPrecondition (migrationPre m) $
                runMigration (migrationUp m) $
                  testPostcondition (migrationPost m) $ do
                    conn <- migrateSettingsBackend <$> ask
                    liftIO $ backendPushMigration conn m
                    return Nothing

-- TODO downMigrateListUI and upMigrateListUI are almost the same
downMigrateListUI :: Backend conn query cond => [Migration query query cond] -> Runner conn query cond (Maybe String)
downMigrateListUI ms = case ms of
  []   -> return Nothing
  m:ms -> do
    msg . MessageMigrationStartedDown . showDownMigration $ m
    err <- downMigrateUI m
    case err of
      Just err -> return $ Just err
      Nothing  -> downMigrateListUI ms

upMigrateListUI :: Backend conn query cond => [Migration query (Maybe query) cond] -> Runner conn query cond (Maybe String)
upMigrateListUI ms = case ms of
  []   -> return Nothing
  m:ms -> do
    msg . MessageMigrationStartedUp . showUpMigration $ m
    err <- upMigrateUI m
    case err of
      Just err -> return $ Just err
      Nothing  -> upMigrateListUI ms

-- TODO better return type
runPlan :: Backend conn query cond => Plan query cond -> Runner conn query cond Bool
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

runMigrations' :: Backend conn query cond => [Migration query (Maybe query) cond] -> Runner conn query cond Bool
runMigrations' migs = do
  conn <- migrateSettingsBackend <$> ask
  ensureTableUI
  olds <- liftIO $ backendGetMigrations conn
  runPlan $ planMigration olds migs

runMigrations :: Backend conn query cond => MigrateSettings conn query cond -> [Migration query (Maybe query) cond] -> IO Bool
runMigrations settings migs = runReaderT (runMigrations' migs) settings
