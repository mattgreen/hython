module Hython.Module
    ( load
    , lookup
    , LoadError (..)
    )
where

import Prelude hiding (lookup)

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

data LoadError
    = SyntaxError String
    | IOError String

load :: (MonadEnv Object m, MonadInterpreter m) => FilePath -> m (Either LoadError ModuleInfo)
load path = do
    currentModulePath   <- takeDirectory . modulePath <$> getCurrentModule
    mpath               <- resolveModulePath path currentModulePath
    case mpath of
        Just canonicalPath -> do
            mmodule <- getModuleByPath canonicalPath
            case mmodule of
                Just info   -> return $ Right info
                Nothing     -> loadFromFile canonicalPath
        Nothing -> return . Left $ IOError "not found"

loadFromFile :: (MonadEnv Object m, MonadInterpreter m) => FilePath -> m (Either LoadError ModuleInfo)
loadFromFile path = do
    code <- liftIO . TIO.readFile $ path
    case parse code of
        Left err    -> return . Left . SyntaxError $ err
        Right stmts -> do
            (Module info)   <- newModule modName path

            current <- getCurrentModule
            setCurrentModule info
            pushModuleEnv (moduleDict info) $
                evalBlock stmts
            setCurrentModule current

            return . Right $ info
  where
    modName = T.pack . takeBaseName $ path

lookup :: (MonadIO m) => Text -> ModuleInfo -> m (Maybe Object)
lookup attr info = do
    dict <- readRef $ moduleDict info
    AttributeDict.lookup attr dict

getLibPath :: MonadIO m => m String
getLibPath = do
    exePath <- fst <$> liftIO splitExecutablePath
    liftIO . canonicalizePath $ exePath `combine` "lib"

resolveModulePath :: MonadIO m => FilePath -> FilePath -> m (Maybe FilePath)
resolveModulePath path currentModulePath = do
    libPath         <- getLibPath
    filename        <- pure $ takeBaseName path ++ ".py"
    possiblePaths   <- pure $ map (`combine` filename) [currentModulePath, libPath]
    mpath           <- firstM (liftIO . doesFileExist) possiblePaths
    case mpath of
        Just p      -> do
            fullPath <- liftIO . canonicalizePath $ p
            return . Just $ fullPath
        Nothing     -> return Nothing

