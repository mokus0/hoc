{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module HOC.CBits.Exceptions where

import Control.Exception
import HOC.CBits.Types
import Foreign.C
import Foreign.Ptr
import Foreign.StablePtr

#ifdef BASE4

foreign import ccall unsafe wrapHaskellException
    :: CString -> StablePtr SomeException -> IO (Ptr ObjCObject)
foreign import ccall unsafe unwrapHaskellException
    :: Ptr ObjCObject -> IO (StablePtr SomeException)

#else
    
foreign import ccall unsafe wrapHaskellException
    :: CString -> StablePtr Exception -> IO (Ptr ObjCObject)
foreign import ccall unsafe unwrapHaskellException
    :: Ptr ObjCObject -> IO (StablePtr Exception)

#endif