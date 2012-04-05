{-# LANGUAGE ForeignFunctionInterface #-}
module HOC.CBits.Marshalling where

import Data.Word
import Foreign.ObjC
import Foreign.Ptr

foreign import ccall safe "Marshalling.h nsStringToUTF8"
    nsStringToUTF8 :: Ptr ObjCObject -> IO (Ptr Word8)

foreign import ccall unsafe "Marshalling.h utf8ToNSString"
    utf8ToNSString :: Ptr Word8 -> IO (Ptr ObjCObject)
