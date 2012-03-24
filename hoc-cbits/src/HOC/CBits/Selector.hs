{-# LANGUAGE ForeignFunctionInterface #-}
module HOC.CBits.Selector where

import HOC.CBits.Types
import Foreign.C

foreign import ccall unsafe "Selector.h getSelectorForName"
    c_getSelectorForName :: CString -> IO SEL  

