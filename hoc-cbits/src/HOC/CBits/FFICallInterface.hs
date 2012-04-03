{-# LANGUAGE ForeignFunctionInterface #-}
module HOC.CBits.FFICallInterface where

import Foreign.LibFFI.Experimental
import Foreign.C.Types

foreign import ccall unsafe cifIsStret :: SomeCIF -> IO CInt