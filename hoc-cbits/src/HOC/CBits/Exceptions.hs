{-# LANGUAGE ForeignFunctionInterface #-}
module HOC.CBits.Exceptions where

import Control.Exception
import Foreign.C
import Foreign.Ptr
import Foreign.StablePtr
import HOC.CBits.Types

foreign import ccall unsafe wrapHaskellException
    :: CString -> StablePtr SomeException -> IO (Ptr ObjCException)
foreign import ccall unsafe unwrapHaskellException
    :: Ptr ObjCException -> IO (StablePtr SomeException)
