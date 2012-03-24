{-# LANGUAGE ForeignFunctionInterface #-}
module HOC.CBits.Invocation where

import HOC.CBits.Types
import HOC.FFICallInterface
import Foreign.Ptr

foreign import ccall "Invocation.h callWithExceptions"
    c_callWithExceptions :: FFICif -> FunPtr a
                        -> Ptr b -> Ptr (Ptr ())
                        -> IO (Ptr ObjCObject) {- NSException -}

