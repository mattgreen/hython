{-# LANGUAGE ScopedTypeVariables #-}

module Hython.Module (loadModule)
where

import Control.Exception
import Control.Monad.State
import Data.List
import System.FilePath.Posix
import System.IO.Error
import Text.Printf

import qualified Hython.AttributeDict as AttributeDict
import Hython.Environment
import Hython.Frame
import Hython.InterpreterState
import Hython.Object

loadModule :: String -> (String -> AttributeDict -> Interpreter ()) -> Interpreter (Either String Module)
loadModule importPath action = do
    current         <- gets currentModule
    loadedModules   <- gets modules

    let resolvedPath = resolveModulePath importPath (modulePath current)

    case lookupModuleByPath resolvedPath loadedModules of
        Just m  -> return $ Right m
        Nothing -> do
            codeOrErr <- liftIO $ try (readFile resolvedPath)
            case codeOrErr of
                Right code -> do
                    dict <- liftIO AttributeDict.empty

                    let newModule = Module {
                        moduleName = newModuleName,
                        modulePath = resolvedPath,
                        moduleDict = dict
                    }

                    env <- currentEnv
                    emptyDict <- liftIO AttributeDict.empty

                    modify $ \s -> s { currentModule = newModule }
                    pushFrame ("File " ++ resolvedPath) Env {
                        localEnv = emptyDict,
                        enclosingEnvs = [],
                        moduleEnv = dict,
                        builtinEnv = builtinEnv env,
                        activeEnv = ModuleEnv
                    }

                    action code dict

                    popFrame
                    modify $ \s -> s { currentModule = current, modules = newModule : modules s }

                    return $ Right newModule

                Left (e::IOError) -> return $ Left (errorMsg e)
  where
    errorMsg e
      | isDoesNotExistError e   = printf "No module named '%s'" newModuleName
      | otherwise               = printf "Unable to load '%s'" newModuleName

    lookupModuleByPath resolvedPath = find (\m -> modulePath m == resolvedPath)
    newModuleName = takeBaseName importPath

resolveModulePath :: String -> FilePath -> FilePath
resolveModulePath path currentModulePath = currentModuleDir `combine` name
  where
    currentModuleDir = takeDirectory currentModulePath
    name = takeBaseName path ++ ".py"
