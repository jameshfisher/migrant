{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module Database.Migrant.Main where

import Control.Exception
import Database.PostgreSQL.Simple


extractSqlError :: IO a -> IO (Either SqlError a)
extractSqlError act = do
  ex <- try act
  case ex of
    Right r -> return $ Right r
    Left ex -> case fromException ex of
      Just ex@(SqlError{..}) -> return $ Left ex
      Nothing                -> throwIO ex

main :: IO ()
main = do

  conn <- connect ConnectInfo
    { connectHost = "127.0.0.1"
    , connectPort = 5432
    , connectUser = "migrate"
    , connectPassword = "migrate"
    , connectDatabase = "migrate"
    }
  
  r <- extractSqlError $ throw (SqlError "gszg" 0 "gdh")
  case r of
    Left e -> putStrLn $ "SqlError: " ++ (show $ sqlErrorMsg e)
    Right f -> putStrLn $ "Okay: " ++ f

  return ()