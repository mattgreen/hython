module Hython.Class
where

import qualified Hython.AttributeDict as AttributeDict
import Hython.Object

classOf :: Object -> Class
classOf (Object cls _) = cls
classOf c = error $ "unexpected: calling classOf on " ++ show c

isSubClass :: Class -> Class -> Bool
isSubClass derived base = base `elem` basesOf derived
  where
    basesOf c = c : concatMap basesOf (classBases c)

newObject :: Class -> IO Object
newObject cls = do
    instanceDict <- AttributeDict.clone (classDict cls)
    AttributeDict.update "__class__" (ClassObj cls) instanceDict

    return $ Object cls instanceDict
