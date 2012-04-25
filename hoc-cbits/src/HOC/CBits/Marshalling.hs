{-# LANGUAGE ForeignFunctionInterface #-}
module HOC.CBits.Marshalling where

import Data.Word
import Foreign.ObjC
import Foreign.Ptr

foreign import ccall safe
    nsStringToUTF8 :: Ptr ObjCObject -> IO (Ptr Word8)

foreign import ccall unsafe
    utf8ToNSString :: Ptr Word8 -> IO (Ptr ObjCObject)
