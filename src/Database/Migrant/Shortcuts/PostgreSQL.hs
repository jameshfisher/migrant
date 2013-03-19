module Database.Migrant.Shortcuts.PostgreSQL
  ( addColumn
  ) where

import Database.Migrant.Data (Migration (..))

addColumn :: String -> String -> String -> Migration String
addColumn table col ty = Migration
  ("alter table " ++ table ++ " add column " ++ col ++ " " ++ ty)
  (Just $ "alter table " ++ table ++ " drop column " ++ col)
  (Just $ "add_column_" ++ table ++ "_" ++ col)
