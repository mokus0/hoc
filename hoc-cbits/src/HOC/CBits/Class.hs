{-# LANGUAGE ForeignFunctionInterface #-}
module HOC.CBits.Class where

import Foreign.C
import Foreign.ObjC
import Foreign.Ptr

foreign import ccall unsafe objc_getClass   :: CString        -> IO (Ptr ObjCObject)
foreign import ccall unsafe object_getClass :: Ptr ObjCObject -> IO (Ptr ObjCObject)

