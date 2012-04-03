{-# LANGUAGE ForeignFunctionInterface #-}
module HOC.CBits.Selector where

import Foreign.C
import Foreign.ObjC.SEL

foreign import ccall unsafe sel_registerName :: CString -> IO SEL  

