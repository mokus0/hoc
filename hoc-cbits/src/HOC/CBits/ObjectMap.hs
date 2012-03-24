{-# LANGUAGE ForeignFunctionInterface #-}
module HOC.CBits.ObjectMap where

import HOC.CBits.Types
import Foreign.C.Types
import Foreign.Ptr
import Foreign.StablePtr
import System.Mem.Weak

-- given a pointer to an ObjCObject, return a stablePtr to a Weak reference to 
-- a HSO
foreign import ccall unsafe "ObjectMap.h getHaskellPart"
    getHaskellPart :: Ptr ObjCObject -> IO (StablePtr (Weak HSO))
-- Sets the mapping of an ObjCObject to the HSO.  The CInt is a boolean flag 
-- used to set whether or not this objective-c object is immortal.  The is 
-- equivelent to saying removeHaskellPart will never be called for this object
foreign import ccall unsafe "ObjectMap.h setHaskellPart"
    setHaskellPart :: Ptr ObjCObject -> StablePtr (Weak HSO) -> CInt -> IO ()
-- remove the objcobject->HSO mapping.  You have to pass both because this 
-- method does nothing if the objCObject maps to a different HSO.  This can 
-- happen if the finalizer ran late and the object was reimported in the 
-- meantime.
foreign import ccall unsafe "ObjectMap.h removeHaskellPart"
    removeHaskellPart :: Ptr ObjCObject -> StablePtr (Weak HSO) -> IO ()
-- returns number of objects allocated in the map and the immortal count in the 
-- two pointers.
foreign import ccall unsafe "ObjectMap.h objectMapStatistics"
    c_objectMapStatistics :: Ptr CUInt -> Ptr CUInt -> IO ()

