{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies,
             TypeSynonymInstances, FlexibleContexts,
             FlexibleInstances #-}
module HOC.Class where

import HOC.Arguments
import HOC.ID
import HOC.MessageTarget
import HOC.CBits

import Foreign.Ptr ( Ptr )
import Foreign.C.String

import System.IO.Unsafe (unsafePerformIO)

unsafeGetClassObject :: String -> Class a

getClassByName name = withCString name c_getClassByName

     -- called from generated code, save space:
{-# NOINLINE unsafeGetClassObject #-}
unsafeGetClassObject name = unsafePerformIO $
    getClassByName name >>= importImmortal

{-# NOINLINE unsafeGetRawClassObject #-}
unsafeGetRawClassObject name = unsafePerformIO $
    getClassByName name


getClassForObject obj = withExportedArgument obj c_getClassForObject


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
            c_getClassForObject (rawStaticClassForObject $ objdummy cls)
        where
            objdummy :: Class a -> ID a
            objdummy = undefined

