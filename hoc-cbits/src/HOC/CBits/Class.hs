{-# LANGUAGE ForeignFunctionInterface #-}
module HOC.CBits.Class where

import HOC.CBits.Types
import Foreign.C
import Foreign.Ptr

foreign import ccall unsafe "Class.h getClassByName"
    c_getClassByName :: CString -> IO (Ptr ObjCObject)

foreign import ccall unsafe "Class.h getClassForObject"
    c_getClassForObject :: Ptr ObjCObject -> IO (Ptr ObjCObject)

