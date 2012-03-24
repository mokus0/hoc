{-# LANGUAGE ForeignFunctionInterface #-}
module HOC.CBits.MemoryManagement where

import HOC.CBits.Types
import Foreign.C.Types
import Foreign.Ptr

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
