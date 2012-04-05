{-# LANGUAGE ForeignFunctionInterface #-}
module HOC.CBits.Invocation where

import Foreign.LibFFI.Experimental
import Foreign.Ptr
import HOC.CBits.Types

foreign import ccall "Invocation.h callWithExceptions"
    c_callWithExceptions :: CIF a -> FunPtr a
                        -> Ptr b -> Ptr (Ptr ())
                        -> IO (Ptr ObjCException)

