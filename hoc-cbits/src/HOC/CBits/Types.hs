{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module HOC.CBits.Types where

import Foreign.Ptr
import Foreign.LibFFI.Experimental
import Foreign.ObjC

type HsIMP a = 
        CIF (Ptr ObjCObject -> SEL a -> a)
     -> Ptr (SigReturn a)
     -> Ptr (Ptr ())
     -> IO ObjCException

foreign import ccall "wrapper"
    wrapHsIMP :: HsIMP a -> IO (FunPtr (HsIMP a))

foreign import ccall
    newIMP :: CIF (Ptr ObjCObject -> SEL a -> a) -> FunPtr (HsIMP a) -> IO (IMP a)
