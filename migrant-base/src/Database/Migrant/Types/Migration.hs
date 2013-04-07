module Database.Migrant.Types.Migration
  ( Migration (..)
  , defaultMigration
  , fromBiMigration
  ) where

data Migration up down cond = Migration
  { migrationUp   :: up
  , migrationDown :: down
  , migrationPre  :: Maybe cond
  , migrationPost :: Maybe cond
  , migrationDescription :: Maybe String
  } deriving (Eq, Show)

defaultMigration :: u -> Migration u (Maybe d) c
defaultMigration up = Migration
  { migrationUp = up
  , migrationDown = Nothing
  , migrationPre = Nothing
  , migrationPost = Nothing
  , migrationDescription = Nothing
  }

fromBiMigration :: Migration up up cond -> Migration up (Maybe up) cond
fromBiMigration (Migration up down pre post desc) = Migration up (Just down) pre post desc
