{-# LANGUAGE ForeignFunctionInterface #-}
module HOC.CBits.MemoryManagement where

import Control.Exception (bracket)
import Foreign.ObjC
import Foreign.Ptr

foreign import ccall "MemoryManagement.h newAutoreleasePool"
    newAutoreleasePool :: IO (Ptr ObjCObject)

withAutoreleasePool :: IO a -> IO a
withAutoreleasePool action = bracket newAutoreleasePool releaseObject (const action)
