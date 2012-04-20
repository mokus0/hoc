{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module HOC.CBits.Types where

import Foreign.ForeignPtr
import Foreign.Ptr
import Foreign.LibFFI.Experimental  hiding (Dynamic)
import Foreign.ObjC                 hiding (ObjCException, Class)
import Foreign.ObjC.HSObject
import System.IO.Unsafe

data ID a = ID {-# UNPACK #-} !HSO | Nil

instance Eq (ID a) where
    (ID (HSO a _)) == (ID (HSO b _))    = a == b
    Nil == Nil                          = True
    _ == _                              = False

instance Show (ID a) where
    showsPrec _ Nil = showString "nil"
    showsPrec _ (ID (HSO fp _))
        = showChar '<'
        . showString cls
        . showChar ' '
        . shows fp
        . showChar '>'
        where cls = unsafePerformIO (withForeignPtr fp object_getClassName)

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

