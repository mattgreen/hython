module Hython.Modules
where

import Language.Python.Core

import Hython.Attributes

newModuleInfo :: String -> IO ModuleInfo
newModuleInfo name = do
    dict <- newAttributeDict []

    return ModuleInfo {
        moduleName = name,
        moduleDict = dict
    }
