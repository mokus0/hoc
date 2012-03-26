{-# LANGUAGE ForeignFunctionInterface #-}
module HOC.CBits.Exceptions where

import Control.Exception
import HOC.CBits.Types
import Foreign.C
import Foreign.Ptr
import Foreign.StablePtr

foreign import ccall unsafe wrapHaskellException
    :: CString -> StablePtr SomeException -> IO (Ptr ObjCObject)
foreign import ccall unsafe unwrapHaskellException
    :: Ptr ObjCObject -> IO (StablePtr SomeException)
