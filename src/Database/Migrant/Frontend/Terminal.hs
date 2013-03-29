module Database.Migrant.Frontend.Terminal
  ( frontendTerminal
  ) where

import System.Console.ANSI (setSGR, SGR(SetColor), ConsoleLayer(Foreground), ColorIntensity(Dull), Color(Green, Yellow, Red))

import Database.Migrant.Data (Message(..))

withColor :: Color -> IO () -> IO ()
withColor color action = do
  setSGR [SetColor Foreground Dull color]
  action
  setSGR []

putSuccess :: String -> IO ()
putSuccess s = withColor Green $ putStrLn s

putWarning :: String -> IO ()
putWarning s = withColor Yellow $ putStrLn s

putError :: String -> IO ()
putError s = withColor Red $ putStrLn s

indent :: String -> String
indent = unlines . map ("    "++) . lines

frontendTerminal :: Message -> IO ()
frontendTerminal m = case m of
  MessageCreatedMigrationStack    -> putStrLn "Created migration stack."
  MessageMigrationStartedUp   mig -> putStrLn $ "Migrating up: "   ++ mig
  MessageMigrationStartedDown mig -> putStrLn $ "Migrating down: " ++ mig
  MessageMigrationCommitted       -> putSuccess "    committed."
  MessageMigrationRolledBack err  -> do
    putError "    rolled back due to error:"
    putError $ indent err
  MessageWarnNoDownMigration      -> putWarning  "    no down-migration provided"
  MessageTestingDownMigration     -> putStrLn    "    testing down-migration"
  MessageWarnNoPrecondition       -> putWarning  "    no precondition provided"
  MessageTestingPrecondition      -> putStrLn    "    testing precondition"
  MessageWarnNoPostCondition      -> putWarning  "    no postcondition provided"
  MessageTestingPostcondition     -> putStrLn    "    testing postcondition"
  MessageMissingDownMigrations ms -> do
    putError "The following down-migrations are not provided:"
    mapM_ (putStrLn . indent) ms
  MessageAborted                  -> putError "Migration halted due to errors."
  MessageCompleted numActions     -> putSuccess $ "Done (" ++ show numActions ++ " actions performed)."
