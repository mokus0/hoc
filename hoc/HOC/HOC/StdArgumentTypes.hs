{-# LANGUAGE TemplateHaskell,
             TypeFamilies, UndecidableInstances,
             TypeSynonymInstances, FlexibleInstances,
             ScopedTypeVariables #-}
module HOC.StdArgumentTypes where

import HOC.CBits
import HOC.Arguments

import Control.Exception        ( bracket )
import Foreign
import Foreign.C.Types
import Foreign.ObjC             ( SEL )

import HOC.Unicode

-- Objective C

-- ID: already defined 

instance ObjCArgument SEL

instance ObjCArgument Bool where
    type ForeignArg Bool = CSChar
    exportArgument False = return 0
    exportArgument True = return 1
    importArgument 0 = return False
    importArgument _ = return True

instance ObjCArgument Int
instance ObjCArgument Float
instance ObjCArgument Double

instance ObjCArgument a => ObjCArgument (Ptr a)

-- Foreign.C.Types

instance ObjCArgument CInt
instance ObjCArgument CUInt

instance ObjCArgument CFloat
instance ObjCArgument CDouble

instance ObjCArgument CChar
instance ObjCArgument CSChar
instance ObjCArgument CUChar

instance ObjCArgument CShort
instance ObjCArgument CUShort

instance ObjCArgument CLong
instance ObjCArgument CULong

instance ObjCArgument CLLong
instance ObjCArgument CULLong

-- String

withUTF8String str = withArray0 0 (unicodeToUtf8 str)

instance (ObjCArgument a, ForeignArg a ~ Ptr b) => ObjCArgument (Maybe a) where
    type ForeignArg (Maybe a) = ForeignArg a
    
    withExportedArgument Nothing  action = action nullPtr
    withExportedArgument (Just x) action = withExportedArgument x action
    exportArgument Nothing  = return nullPtr
    exportArgument (Just x) = exportArgument x
    importArgument p
        | p == nullPtr  = return Nothing
        | otherwise     = fmap Just (importArgument p)

instance ObjCArgument String where
    type ForeignArg String = Ptr ObjCObject
    
    withExportedArgument arg action =
        bracket (withUTF8String arg utf8ToNSString) releaseObject action
    exportArgument arg = do
        nsstr <- withUTF8String arg utf8ToNSString
        autoreleaseObject nsstr
        return nsstr
    importArgument arg = nsStringToUTF8 arg >>= peekArray0 0
                         >>= return . utf8ToUnicode
