module Database.Migrant.Types.Migration
  ( Migration (..)
  ) where

data Migration up down cond = Migration
  { migrationUp   :: up
  , migrationDown :: down
  , migrationPre  :: Maybe cond
  , migrationPost :: Maybe cond
  , migrationDescription :: Maybe String
  } deriving (Eq, Show)
