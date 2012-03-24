{-# LANGUAGE ForeignFunctionInterface #-}
module HOC.CBits.NewClass where

import Data.Word
import HOC.CBits.Types
import HOC.FFICallInterface
import Foreign.C
import Foreign.Ptr

foreign import ccall "NewClass.h newClass"
    rawNewClass :: Ptr ObjCObject -> CString
             -> Ptr IvarList
             -> Ptr MethodList -> Ptr MethodList
             -> IO ()

foreign import ccall "NewClass.h makeMethodList"
    rawMakeMethodList :: Int -> IO (Ptr MethodList)
foreign import ccall "NewClass.h setMethodInList"
    rawSetMethodInList :: Ptr MethodList -> Int
                    -> SEL -> CString
                    -> FFICif -> FunPtr IMP
                    -> IO ()

                      
foreign import ccall "NewClass.h makeIvarList"
    rawMakeIvarList :: Int -> IO (Ptr IvarList)
foreign import ccall "NewClass.h setIvarInList"
    rawSetIvarInList :: Ptr IvarList -> Int
                  -> CString -> CString -> CSize -> Word8 -> IO ()
