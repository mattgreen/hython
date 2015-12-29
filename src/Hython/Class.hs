module Hython.Class (isSubClass, lookup)
where

import Prelude hiding (lookup)
import Control.Monad.IO.Class (MonadIO)
import Data.List (find)
import Data.Text (Text)
import Safe (tailDef)

import qualified Hython.AttributeDict as AttributeDict
import Hython.Ref
import Hython.Types

lookup :: (MonadIO m) => Text -> ClassInfo -> m (Maybe Object)
lookup attr cls = do
    dicts <- mapM (readRef . classDict) (basesOf cls)
    lookupAttr dicts
  where
    lookupAttr :: (MonadIO m) => [AttributeDict] -> m (Maybe Object)
    lookupAttr (d:ds) = do
        mobj <- AttributeDict.lookup attr d
        case mobj of
            Just obj    -> return $ Just obj
            Nothing     -> lookupAttr ds
    lookupAttr [] = return Nothing

basesOf :: ClassInfo -> [ClassInfo]
basesOf c = c : c3Merge (parentLinearizations ++ [bases])
  where
    bases = classBases c
    parentLinearizations = map basesOf bases

isSubClass :: ClassInfo -> ClassInfo -> Bool
isSubClass derived base = base `elem` basesOf derived

c3Merge :: [[ClassInfo]] -> [ClassInfo]
c3Merge [[]] = []
c3Merge [[c]] = [c]
c3Merge cs = case base of
    Just b  -> b : c3Merge (remaining b)
    Nothing -> []
  where
    base = nextEligibleBase cs
    remaining b = map (dropWhile (== b)) cs

nextEligibleBase :: [[ClassInfo]] -> Maybe ClassInfo
nextEligibleBase classes = find nextBestBase possibleBases
  where
    possibleBases = concat classes
    nextBestBase c = all (\cs -> c `notElem` tailDef [] cs) classes

