{-# LANGUAGE ForeignFunctionInterface #-}
module HOC.CBits.RetainedHaskellPart where

import HOC.CBits.Types
import Foreign.Ptr
import Foreign.StablePtr

foreign import ccall unsafe "RetainedHaskellPart.h getRetainedHaskellPart"
    getRetainedHaskellPart :: Ptr ObjCObject -> IO (StablePtr HSO)
foreign import ccall unsafe "RetainedHaskellPart.h setRetainedHaskellPart"
    setRetainedHaskellPart :: Ptr ObjCObject -> StablePtr HSO -> IO ()
