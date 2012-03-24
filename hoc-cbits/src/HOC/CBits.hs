{-# LANGUAGE ForeignFunctionInterface #-}
module HOC.CBits
    ( module HOC.CBits
        
    , module HOC.CBits.Class
    , module HOC.CBits.Exceptions
    , module HOC.CBits.GetNewHaskellData
    , module HOC.CBits.Invocation
    , module HOC.CBits.Marshalling
    , module HOC.CBits.MemoryManagement
    , module HOC.CBits.MsgSend
    , module HOC.CBits.NewClass
    , module HOC.CBits.ObjectMap
    , module HOC.CBits.RetainedHaskellPart
    , module HOC.CBits.Selector
    , module HOC.CBits.Statistics
    , module HOC.CBits.Types
    ) where

import HOC.CBits.Class
import HOC.CBits.Exceptions
import HOC.CBits.GetNewHaskellData
import HOC.CBits.Invocation
import HOC.CBits.Marshalling
import HOC.CBits.MemoryManagement
import HOC.CBits.MsgSend
import HOC.CBits.NewClass
import HOC.CBits.ObjectMap
import HOC.CBits.RetainedHaskellPart
import HOC.CBits.Selector
import HOC.CBits.Statistics
import HOC.CBits.Types

import Foreign.Ptr

foreign import ccall "stdlib.h &free"
    freePtr :: FunPtr (Ptr a -> IO ())

