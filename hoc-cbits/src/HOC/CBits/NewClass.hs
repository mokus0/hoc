{-# LANGUAGE ForeignFunctionInterface #-}
module HOC.CBits.NewClass where

import Data.Word
import Foreign.C
import Foreign.LibFFI.Experimental
import Foreign.ObjC
import Foreign.ObjC.SEL
import Foreign.Ptr
import HOC.CBits.Types

foreign import ccall "NewClass.h newClass"
    rawNewClass :: Ptr ObjCObject -> CString
             -> Ptr IvarList
             -> Ptr MethodList -> Ptr MethodList
             -> IO ()

foreign import ccall "NewClass.h makeMethodList"
    rawMakeMethodList :: Int -> IO (Ptr MethodList)
foreign import ccall "NewClass.h setMethodInList"
    rawSetMethodInList :: Ptr MethodList -> Int
                    -> SEL a -> CString
                    -> CIF (Ptr ObjCObject -> SEL a -> a) -> FunPtr (HsIMP a)
                    -> IO ()

                      
foreign import ccall "NewClass.h makeIvarList"
    rawMakeIvarList :: Int -> IO (Ptr IvarList)
foreign import ccall "NewClass.h setIvarInList"
    rawSetIvarInList :: Ptr IvarList -> Int
                  -> CString -> CString -> CSize -> Word8 -> IO ()
