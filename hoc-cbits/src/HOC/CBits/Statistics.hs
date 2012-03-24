{-# LANGUAGE ForeignFunctionInterface #-}
module HOC.CBits.Statistics where

import Foreign.C.Types
import Foreign.Ptr

foreign import ccall unsafe "Statistics.h recordHOCEvent" recordHOCEvent :: CInt -> Ptr (Ptr ()) -> IO ()
