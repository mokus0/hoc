{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies,
             TypeSynonymInstances, FlexibleContexts,
             FlexibleInstances #-}
module HOC.Class
    ( ClassAndObject
    , ClassObject(..)
    , unsafeGetClassObject
    , RawStaticClass(..)
    , unsafeGetRawClassObject
    ) where

import Foreign.ObjC         ( ObjCClass, objc_getClass, object_getClass )
import Foreign.Ptr          ( Ptr, castPtr )
import HOC.CBits            ( Class, ID )
import HOC.ID               ( importClass )
import HOC.MessageTarget    ( Object )
import System.IO.Unsafe     ( unsafePerformIO )

class (Object a, Object b) => ClassAndObject a b | a -> b, b -> a

instance ClassAndObject (Class a) (ID a)

class Object cls => ClassObject cls
    where
        classObject :: cls

-- called from generated code, save space:
unsafeGetClassObject :: String -> Class a
unsafeGetClassObject name = unsafePerformIO $
    importClass (unsafeGetRawClassObject name)


class Object a => RawStaticClass a where
    rawStaticClassForObject :: a -> Ptr ObjCClass

instance RawStaticClass (ID a) => RawStaticClass (Class a) where
    rawStaticClassForObject = unsafePerformIO . object_getClass
        . castPtr . rawStaticClassForObject . objdummy
        where
            objdummy :: Class a -> ID a
            objdummy = undefined

unsafeGetRawClassObject :: String -> Ptr ObjCClass
unsafeGetRawClassObject = unsafePerformIO . objc_getClass
