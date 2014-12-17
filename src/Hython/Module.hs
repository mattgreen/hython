module Hython.Module
where

import System.FilePath.Posix

getModuleName :: FilePath -> String
getModuleName path = takeBaseName path

getModulePath :: FilePath -> String -> String
getModulePath currentModulePath name = currentModuleDir `combine` filename
  where
    currentModuleDir = takeDirectory currentModulePath
    filename = (replace name '.' '/') ++ ".py"
    replace s c r = map (\x -> if x == c then r else x) s
