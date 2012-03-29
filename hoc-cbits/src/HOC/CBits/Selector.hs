{-# LANGUAGE ForeignFunctionInterface #-}
module HOC.CBits.Selector where

import HOC.CBits.Types
import Foreign.C

foreign import ccall unsafe sel_registerName :: CString -> IO SEL  

