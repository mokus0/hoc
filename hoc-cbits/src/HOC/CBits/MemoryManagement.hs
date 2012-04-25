{-# LANGUAGE ForeignFunctionInterface #-}
module HOC.CBits.MemoryManagement where

import Control.Exception (bracket)
import Foreign.ObjC
import Foreign.Ptr

foreign import ccall
    newAutoreleasePool :: IO (Ptr ObjCObject)

withAutoreleasePool :: IO a -> IO a
withAutoreleasePool = bracket newAutoreleasePool releaseObject . const
