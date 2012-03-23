{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls, GeneralizedNewtypeDeriving #-}
module HOC.Base where

import Control.Exception(bracket)
import Foreign.C.String (CString, withCString)
import Foreign.Ptr (Ptr, FunPtr)
import Foreign.Storable (Storable)
import System.IO.Unsafe (unsafePerformIO)

-- The SEL Type

newtype SEL = SEL (Ptr ())
    deriving(Storable)

foreign import ccall unsafe "Selector.h getSelectorForName"
    c_getSelectorForName :: CString -> IO SEL  

getSelectorForName :: String -> SEL
getSelectorForName str = unsafePerformIO $
    withCString str (c_getSelectorForName)

--

foreign import ccall "stdlib.h &free"
    freePtr :: FunPtr (Ptr a -> IO ())

--

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

withAutoreleasePool action = bracket newAutoreleasePool releaseObject (const action)

--

data ObjCObject
