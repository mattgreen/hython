module Hython.Scoping
where

import Control.Monad.State
import qualified Data.HashMap.Strict as Map
import Text.Printf

import Language.Python.Core
import Hython.Environment

currentScope :: Evaluator SymbolTable
currentScope = do
    current <- gets scopes
    return $ head current

lookupSymbol :: String -> Evaluator (Maybe Value)
lookupSymbol name = do
    scope <- currentScope
    case Map.lookup name scope of
        Just v  -> return $ Just v
        Nothing -> do
            builtinSymbols <- gets builtins
            return $ lookup name builtinSymbols

removeSymbol :: String -> Evaluator ()
removeSymbol name = do
    scope <- currentScope

    let updatedScope = Map.delete name scope
    modify $ \env -> env { scopes = updatedScope : tail (scopes env) }

updateSymbol :: String -> Value -> Evaluator ()
updateSymbol name value = do
    scope <- currentScope

    let updatedScope = Map.insert name value scope
    modify $ \env -> env { scopes = updatedScope : tail (scopes env) }
