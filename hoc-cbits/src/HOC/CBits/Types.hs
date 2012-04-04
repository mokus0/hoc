{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module HOC.CBits.Types where

import Data.Dynamic
import Foreign.ForeignPtr
import Foreign.Ptr
import Foreign.LibFFI.Experimental hiding (Dynamic)
import Foreign.ObjC

data ID a = ID HSO | Nil

instance Eq (ID a) where
    (ID (HSO a _)) == (ID (HSO b _))    = a == b
    Nil == Nil                          = True
    _ == _                              = False

nil :: ID a
nil = Nil

castObject :: ID a -> ID b
castObject (ID a) = ID a
castObject Nil = Nil

-- HSO: HaskellSideObject
data HSO = HSO (Ptr ObjCObject) [Dynamic]

-- an abstract type used to label 'Ptr's
data ObjCObject

instance FFIType ObjCObject where
    ffiType = castType pointer
instance ObjCType ObjCObject where
    typeString    _ = "@"
    ptrTypeString _ = "@"

data Class_ a
type Class a = ID (Class_ a)
type MetaClass a = Class (Class_ a)

type HsIMP a = CIF (Ptr ObjCObject -> SEL -> a) -> Ptr (SigReturn a) -> Ptr (Ptr ()) -> IO (Ptr ObjCObject {- NSException -})
foreign import ccall "wrapper" wrapHsIMP :: HsIMP a -> IO (FunPtr (HsIMP a))

newtype MethodList = MethodList (ForeignPtr MethodList)
newtype IvarList = IvarList (ForeignPtr IvarList)

