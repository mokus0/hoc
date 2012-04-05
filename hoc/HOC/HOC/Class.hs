{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies,
             TypeSynonymInstances, FlexibleContexts,
             FlexibleInstances #-}
module HOC.Class where

import Foreign.C.String     ( withCString )
import Foreign.ObjC         ( ObjCObject )
import Foreign.Ptr          ( Ptr )
import HOC.Arguments        ( withExportedArgument )
import HOC.CBits            ( Class, objc_getClass, ID, object_getClass )
import HOC.ID               ( importImmortal )
import HOC.MessageTarget    ( Object )
import System.IO.Unsafe     ( unsafePerformIO )

unsafeGetClassObject :: String -> Class a

getClassByName name = withCString name objc_getClass

     -- called from generated code, save space:
{-# NOINLINE unsafeGetClassObject #-}
unsafeGetClassObject name = unsafePerformIO $
    getClassByName name >>= importImmortal

{-# NOINLINE unsafeGetRawClassObject #-}
unsafeGetRawClassObject name = unsafePerformIO $
    getClassByName name


getClassForObject obj = withExportedArgument obj object_getClass


class (Object a, Object b) => ClassAndObject a b | a -> b, b -> a

instance ClassAndObject (Class a) (ID a)

class Object cls => ClassObject cls
    where
        classObject :: cls

class Object a => RawStaticClass a where
    rawStaticClassForObject :: a -> Ptr ObjCObject

instance RawStaticClass (ID a) => RawStaticClass (Class a) where
    rawStaticClassForObject cls = 
        unsafePerformIO $
            object_getClass (rawStaticClassForObject $ objdummy cls)
        where
            objdummy :: Class a -> ID a
            objdummy = undefined

