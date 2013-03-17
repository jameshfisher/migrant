module Database.PostgreSQL.Migrate.Terminal
  ( putSuccess
  , putError
  ) where

import System.Console.ANSI (setSGR, SGR(SetColor), ConsoleLayer(Foreground), ColorIntensity(Dull), Color(..))

withColor :: Color -> IO () -> IO ()
withColor color action = do
  setSGR [SetColor Foreground Dull color]
  action
  setSGR []

putSuccess :: String -> IO ()
putSuccess s = withColor Green $ putStrLn s

putError :: String -> IO ()
putError s = withColor Red $ putStrLn s
