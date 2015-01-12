module Hython.Class
where

import Data.List

import Safe

import qualified Hython.AttributeDict as AttributeDict
import Hython.Object

classOf :: Object -> Class
classOf (Object cls _) = cls
classOf (ClassObj cls) = cls
classOf c = error $ "unexpected: calling classOf on " ++ show c

isSubClass :: Class -> Class -> Bool
isSubClass derived base = base `elem` basesOf derived

lookupAttr :: String -> Class -> IO (Maybe Object)
lookupAttr name cls = lookup (map classDict (basesOf cls))
  where
    lookup [] = return Nothing
    lookup (d:ds) = do
        obj <- AttributeDict.lookup name d
        case obj of
            Just o  -> return $ Just o
            Nothing -> lookup ds

basesOf :: Class -> [Class]
basesOf c = c : c3Merge (parentLinearizations ++ [bases])
  where
    bases = classBases c
    parentLinearizations = map basesOf bases

c3Merge :: [[Class]] -> [Class]
c3Merge [[]] = []
c3Merge [[c]] = [c]
c3Merge cs = case base of
    Just b  -> b : c3Merge (remaining b)
    Nothing -> []
  where
    base = nextEligibleBase cs
    remaining b = map (dropWhile (== b)) cs

nextEligibleBase :: [[Class]] -> Maybe Class
nextEligibleBase classes = case find nextBestBase possibleBases of
    Just c  -> Just c
    Nothing -> Nothing
  where
    possibleBases = concat classes
    nextBestBase c = all (\cs -> c `notElem` tailDef [] cs) classes

newObject :: Class -> IO Object
newObject cls = do
    instanceDict <- AttributeDict.empty
    AttributeDict.update "__class__" (ClassObj cls) instanceDict

    return $ Object cls instanceDict
