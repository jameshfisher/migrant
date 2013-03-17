module Shortcuts (addColumn) where

import Data

addColumn :: String -> String -> String -> Migration
addColumn table col ty = Migration
  ("add_column_" ++ table ++ "_" ++ col)
  ("alter table " ++ table ++ " add column " ++ col ++ " " ++ ty)
  (Just $ "alter table " ++ table ++ " drop column " ++ col)
