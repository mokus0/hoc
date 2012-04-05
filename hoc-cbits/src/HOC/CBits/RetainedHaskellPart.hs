{-# LANGUAGE ForeignFunctionInterface #-}
module HOC.CBits.RetainedHaskellPart where

import Foreign.ObjC
import Foreign.Ptr
import Foreign.StablePtr
import HOC.CBits.Types

foreign import ccall unsafe "RetainedHaskellPart.h getRetainedHaskellPart"
    getRetainedHaskellPart :: Ptr ObjCObject -> IO (StablePtr HSO)
foreign import ccall unsafe "RetainedHaskellPart.h setRetainedHaskellPart"
    setRetainedHaskellPart :: Ptr ObjCObject -> StablePtr HSO -> IO ()
