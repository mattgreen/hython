module Hython.Object
where

import Data.ByteString
import Data.Complex
import Data.IORef

data Object = None
            | Bool Bool
            | Bytes ByteString
            | Float Double
            | Imaginary (Complex Double)
            | Int Integer
            | String String
            deriving (Eq, Show)

type ObjectRef = IORef Object

