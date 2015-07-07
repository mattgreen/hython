module Hython.Object
where

import Data.IORef

data Object = Object
type ObjectRef = IORef Object
