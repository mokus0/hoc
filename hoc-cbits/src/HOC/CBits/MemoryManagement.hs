{-# LANGUAGE ForeignFunctionInterface #-}
module HOC.CBits.MemoryManagement where

import Control.Exception (bracket)
import Foreign.C.Types
import Foreign.ObjC
import Foreign.Ptr

foreign import ccall "MemoryManagement.h retainObject"
    retainObject :: Ptr ObjCObject -> IO ()
foreign import ccall "MemoryManagement.h releaseObject"
    releaseObject :: Ptr ObjCObject -> IO ()
foreign import ccall "MemoryManagement.h deallocObject"
    deallocObject :: Ptr ObjCObject -> IO ()

foreign import ccall "MemoryManagement.h autoreleaseObject"
    autoreleaseObject :: Ptr ObjCObject -> IO ()
foreign import ccall "MemoryManagement.h newAutoreleasePool"
    newAutoreleasePool :: IO (Ptr ObjCObject)

foreign import ccall "MemoryManagement.h retainSuper"
    retainSuper :: Ptr ObjCObject -> Ptr ObjCObject -> IO ()
foreign import ccall "MemoryManagement.h releaseSuper"
    releaseSuper :: Ptr ObjCObject -> Ptr ObjCObject -> IO ()
foreign import ccall unsafe "MemoryManagement.h retainCount"
    retainCount :: Ptr ObjCObject -> IO CUInt

-- Since finalizers are executed in arbitrary threads, we must
-- ensure that we establish an autoreleasepool for the duration
-- of the execution of the release/dealloc messages.
-- Since the execution of each finalizer might get spread out
-- over several native threads, we perform the operation
-- together with pool allocation in c, to avoid allocating the
-- pool in one thread, executing the release/dealloc in a second
-- and freeing the pool in a third.
foreign import ccall "MemoryManagement.h releaseObjectWithPool"
    releaseObjectWithPool :: Ptr ObjCObject -> IO ()
foreign import ccall "MemoryManagement.h deallocObjectWithPool"
    deallocObjectWithPool :: Ptr ObjCObject -> IO ()

withAutoreleasePool :: IO a -> IO a
withAutoreleasePool action = bracket newAutoreleasePool releaseObject (const action)
