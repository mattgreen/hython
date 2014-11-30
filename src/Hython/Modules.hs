module Hython.Modules
where

import Language.Python.Core

import qualified Hython.AttributeDict as AttributeDict

newModuleInfo :: String -> IO ModuleInfo
newModuleInfo name = do
    dict <- AttributeDict.empty

    return ModuleInfo {
        moduleName = name,
        moduleDict = dict
    }
