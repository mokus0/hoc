{-# LANGUAGE ForeignFunctionInterface #-}
module HOC.CBits.Invocation where

import HOC.CBits.Types
import Foreign.LibFFI.Experimental
import Foreign.Ptr

foreign import ccall "Invocation.h callWithExceptions"
    c_callWithExceptions :: CIF a -> FunPtr a
                        -> Ptr b -> Ptr (Ptr ())
                        -> IO (Ptr ObjCObject) {- NSException -}

