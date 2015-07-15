module Hython.Object
where

import Data.ByteString (ByteString)
import Data.Complex (Complex)
import Data.IORef (IORef)

data Object = None
            | Bool Bool
            | Bytes ByteString
            | Float Double
            | Imaginary (Complex Double)
            | Int Integer
            | String String
            | BuiltinFn String

type ObjectRef = IORef Object
