module Hython.Module
where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Loops (firstM)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import System.Directory (canonicalizePath, doesFileExist)
import System.Environment.Executable (splitExecutablePath)
import System.FilePath.Posix (combine, takeBaseName, takeDirectory)

import Language.Python.Parser (parse)

import qualified Hython.AttributeDict as AttributeDict
import Hython.Environment
import Hython.Ref
import Hython.Types

data ModuleLoadError
    = SyntaxError String
    | IOError String

load :: (MonadEnv Object m, MonadInterpreter m) => Text -> FilePath -> m (Either ModuleLoadError ModuleInfo)
load name path = do
    currentModulePath   <- takeDirectory . modulePath <$> getCurrentModule
    mpath               <- resolveModulePath path currentModulePath
    case mpath of
        Just canonicalPath -> do
            mmodule <- getModuleByPath canonicalPath
            case mmodule of
                Just info   -> return $ Right info
                Nothing     -> loadFromFile name canonicalPath
        Nothing -> return . Left $ IOError "not found"

loadFromFile :: (MonadEnv Object m, MonadInterpreter m) => Text -> FilePath -> m (Either ModuleLoadError ModuleInfo)
loadFromFile name path = do
    code <- liftIO . TIO.readFile $ path
    case parse code of
        Left err    -> return . Left . SyntaxError $ err
        Right stmts -> do
            (Module info)   <- newModule name path

            current <- getCurrentModule
            setCurrentModule info
            pushModuleEnv $
                evalBlock stmts
            setCurrentModule current

            return . Right $ info

getLibPath :: MonadIO m => m String
getLibPath = do
    exePath <- fst <$> liftIO splitExecutablePath
    liftIO . canonicalizePath $ exePath `combine` "lib"

resolveModulePath :: MonadIO m => FilePath -> FilePath -> m (Maybe FilePath)
resolveModulePath path currentModulePath = do
    libPath         <- getLibPath
    filename        <- pure $ (takeBaseName path) ++ ".py"
    possiblePaths   <- pure $ map (`combine` filename) [currentModulePath, libPath]
    mpath           <- firstM (liftIO . doesFileExist) possiblePaths
    case mpath of
        Just path   -> do
            fullPath <- liftIO . canonicalizePath $ path
            return . Just $ fullPath
        Nothing     -> return Nothing

