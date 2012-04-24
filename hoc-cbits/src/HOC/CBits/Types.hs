{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module HOC.CBits.Types where

import Foreign.Ptr
import Foreign.LibFFI.Experimental  hiding (Dynamic)
import Foreign.ObjC                 hiding (ObjCException, Class)
import Foreign.ObjC.HSObject

-- As much as I'd like to make the HSO an unpacked strict field,
-- that breaks memory management because weak pointers to the HSO
-- get finalized as soon as the HSO gets put into an ID.
data ID a = ID HSO | Nil
    deriving (Eq)

instance Show (ID a) where
    showsPrec _  Nil     = showString "nil"
    showsPrec p (ID hso) = showsPrec p hso

nil :: ID a
nil = Nil

castObject :: ID a -> ID b
castObject (ID a) = ID a
castObject Nil = Nil

data Class_ a
type Class a = ID (Class_ a)
type MetaClass a = Class (Class_ a)

newtype ObjCException = ObjCException ObjCObject
    deriving (ObjCType)

type HsIMP a = CIF (Ptr ObjCObject -> SEL a -> a) -> Ptr (SigReturn a) -> Ptr (Ptr ()) -> IO (Ptr ObjCException)
foreign import ccall "wrapper" wrapHsIMP :: HsIMP a -> IO (FunPtr (HsIMP a))
foreign import ccall newIMP :: CIF (Ptr ObjCObject -> SEL a -> a) -> FunPtr (HsIMP a) -> IO (IMP a)

