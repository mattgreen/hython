{-# LANGUAGE ScopedTypeVariables #-}

module Hython.Module (loadModule)
where

import Control.Exception
import Control.Monad.Loops
import Control.Monad.State
import Data.List
import System.Directory
import System.Environment.Executable
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
    let name = takeBaseName importPath

    current         <- gets currentModule
    resolvedPath    <- liftIO $ resolveModulePath importPath (modulePath current)

    case resolvedPath of
        Just path   -> loadModuleInFile path name action
        Nothing     -> return $ Left $ printf "No module named '%s'" name

loadModuleInFile :: String -> String -> (String -> AttributeDict -> Interpreter ()) -> Interpreter (Either String Module)
loadModuleInFile path name action = do
    current         <- gets currentModule
    loadedModules   <- gets modules

    case lookupModuleByPath path loadedModules of
        Just m  -> return $ Right m
        Nothing -> do
            codeOrErr <- liftIO $ try (readFile path)
            case codeOrErr of
                Right code -> do
                    dict <- liftIO AttributeDict.empty

                    let newModule = Module {
                        moduleName = name,
                        modulePath = path,
                        moduleDict = dict
                    }

                    env <- currentEnv
                    emptyDict <- liftIO AttributeDict.empty

                    modify $ \s -> s { currentModule = newModule }
                    pushFrame ("File " ++ path) Env {
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
      | isDoesNotExistError e   = printf "No module named '%s'" name
      | otherwise               = printf "Unable to load '%s'" name

    lookupModuleByPath resolvedPath = find (\m -> modulePath m == resolvedPath)

resolveModulePath :: String -> String -> IO (Maybe FilePath)
resolveModulePath path currentModulePath = do
    libPath <- getLibPath
    let possiblePaths = map (`combine` moduleFilename) [currentModuleDir, libPath]
    firstM doesFileExist possiblePaths

  where
    currentModuleDir = takeDirectory currentModulePath
    moduleFilename = takeBaseName path ++ ".py"
    getLibPath = do
        (executablePath, _) <- splitExecutablePath
        canonicalizePath (executablePath `combine` "lib")

