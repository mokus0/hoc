{-# LANGUAGE ForeignFunctionInterface #-}
module HOC.CBits
    ( module HOC.CBits
        
    , module HOC.CBits.Exceptions
    , module HOC.CBits.FFICallInterface
    , module HOC.CBits.Invocation
    , module HOC.CBits.Marshalling
    , module HOC.CBits.MemoryManagement
    , module HOC.CBits.MsgSend
    , module HOC.CBits.Statistics
    , module HOC.CBits.Types
    ) where

import HOC.CBits.Exceptions
import HOC.CBits.FFICallInterface
import HOC.CBits.Invocation
import HOC.CBits.Marshalling
import HOC.CBits.MemoryManagement
import HOC.CBits.MsgSend
import HOC.CBits.Statistics
import HOC.CBits.Types

import Foreign.Ptr

foreign import ccall "stdlib.h &free"
    freePtr :: FunPtr (Ptr a -> IO ())

