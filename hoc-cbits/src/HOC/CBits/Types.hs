{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module HOC.CBits.Types where

import Data.Dynamic
import Foreign.ForeignPtr
import Foreign.Ptr
import Foreign.Storable
import HOC.FFICallInterface

data ID a = ID HSO | Nil

instance Eq (ID a) where
    (ID (HSO a _)) == (ID (HSO b _))    = a == b
    Nil == Nil                          = True
    _ == _                              = False

-- HSO: HaskellSideObject
data HSO = HSO (Ptr ObjCObject) [Dynamic]

-- an abstract type used to label 'Ptr's
data ObjCObject

data Class_ a
type Class a = ID (Class_ a)
type MetaClass a = Class (Class_ a)

newtype SEL = SEL (Ptr ())
    deriving (Storable)

type IMP = FFICif -> Ptr () -> Ptr (Ptr ()) -> IO (Ptr ObjCObject)
foreign import ccall "wrapper" wrapIMP :: IMP -> IO (FunPtr IMP)

newtype MethodList = MethodList (ForeignPtr MethodList)
newtype IvarList = IvarList (ForeignPtr IvarList)

