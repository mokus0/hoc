{-# LANGUAGE TemplateHaskell, EmptyDataDecls, TypeFamilies,
             FlexibleContexts, ScopedTypeVariables #-}
module HOC.Arguments where

import Foreign.LibFFI.Experimental
import Foreign.Storable
import Foreign.ObjC
import Foreign.Ptr
import Foreign.Marshal.Array
import System.IO.Unsafe(unsafePerformIO)

import HOC.TH

-- ObjCArgument is the type class for Objective C arguments.
-- exportArgument is the FFIType type used when exporting this type.
-- importArgument is the FFIType used when importing this type
-- objCTypeString is the type string that should be used to identify this type 
-- to the objective-c runtime.
class RetType (ForeignArg a) => ObjCArgument a where
    type ForeignArg a
    type ForeignArg a = a
    
    withExportedArgument :: a -> (ForeignArg a -> IO c) -> IO c
    exportArgument :: a -> IO (ForeignArg a)
    exportArgumentRetained :: a -> IO (ForeignArg a)
    importArgument :: ForeignArg a -> IO a
    
    objCTypeString :: a -> String
    
    withExportedArgument arg action = exportArgument arg >>= action
    
    exportArgumentRetained = exportArgument
{-
    For types that are Storable & FFITypeable, define
    
    instance ObjCArgument MyType MyType where
        withExportedArgument = flip ($)
        exportArgument = return
        importArgument = return 
-}

declareStorableObjCArgument :: TypeQ -> String -> Q [Dec]

{- This is what we'd like to do.
declareStorableObjCArgument ty str =
    [d| instance ObjCArgument $(ty) where
            exportArgument = return
            importArgument = return
            objCTypeString = str
    |]
-}

declareStorableObjCArgument ty str = do
    argInst <- instanceD (cxt []) (conT ''ObjCArgument `appT` ty)
            `whereQ` [d|
                {- withExportedArgument = flip ($) -}
                exportArgument x = return x
                importArgument x = return x
                objCTypeString _ = str
            |]
    return [argInst]

instance ObjCArgument () where
    exportArgument = undefined
    importArgument = undefined
    objCTypeString _ = "v"

type family ForeignSig a
type instance ForeignSig (IO a) = IO (ForeignArg a)
type instance ForeignSig (a -> b) = ForeignArg a -> ForeignSig b

-- This creates the ffi_cif (or the haskell binding of it) for a given 
-- ObjCIMPType.  This is the heart of the glue for the language binding.
getCifForSelector :: SigType (ForeignSig a) => a -> SomeCIF
getCifForSelector sel = getCIF defaultABI ret orderedArgs
    where
        proxy = (const Nothing :: a -> Maybe (ForeignSig a)) sel
        ret  = retTypeOf  proxy
        args = argTypesOf proxy
        selType = ffiTypeOf_ ([] :: [SEL])
        orderedArgs = last args : selType : init args

getCifForFunction :: SigType (ForeignSig a) => a -> CIF (ForeignSig a)
getCifForFunction fun = cif

-- This creates an objective-c type signature for a given ObjCIMPType
objCMethodType :: ObjCSigType (ForeignSig a) => a -> String
objCMethodType thing = ret ++ concat (last args : ":" : init args)
    where
        proxy = (const Nothing :: a -> Maybe (ForeignSig a)) thing
        ret  = retTypeString  proxy
        args = argTypeStrings proxy
