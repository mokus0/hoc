{-# LANGUAGE TemplateHaskell, EmptyDataDecls, TypeFamilies,
             FlexibleContexts, ScopedTypeVariables, DefaultSignatures #-}
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
-- to the objective-c runtime.
class FFIType (ForeignArg a) => ObjCArgument a where
    type ForeignArg a
    type ForeignArg a = a
    
    importArgument :: ForeignArg a -> IO a
    default importArgument :: ForeignArg a ~ a => a -> IO a
    importArgument = return
    
    exportArgument :: a -> IO (ForeignArg a)
    default exportArgument :: ForeignArg a ~ a => a -> IO a
    exportArgument = return
    
    exportArgumentRetained :: a -> IO (ForeignArg a)
    exportArgumentRetained = exportArgument
    
    withExportedArgument :: a -> (ForeignArg a -> IO c) -> IO c
    withExportedArgument arg action = exportArgument arg >>= action

withMarshalledArgument :: (ObjCArgument a, ArgType (ForeignArg a)) => a -> (Ptr (ForeignArg a) -> IO c) -> IO c
withMarshalledArgument = withOutArg objcOutArg

objcInRet :: (ObjCArgument a, RetType (ForeignArg a)) => InRet (ForeignArg a) a
objcInRet = inRet { peekRet = \p -> peekRet inRet p >>= importArgument }

objcOutRet :: (ObjCArgument a, RetType (ForeignArg a)) => Bool -> OutRet (ForeignArg a) a
objcOutRet True  = OutRet $ \p x -> exportArgumentRetained x >>= pokeRet outRet p
objcOutRet False = OutRet $ \p x -> exportArgument         x >>= pokeRet outRet p

objcInArg :: (ObjCArgument a, ArgType (ForeignArg a)) => InArg (ForeignArg a) a
objcInArg = InArg { peekArg = \p -> peekArg inArg p >>= importArgument }

objcOutArg :: (ObjCArgument a, ArgType (ForeignArg a)) => OutArg (ForeignArg a) a
objcOutArg = OutArg $ \x action -> withExportedArgument x $ \y ->
    withOutArg outArg y action

instance ObjCArgument ()

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

-- This creates an objective-c type signature for a given ObjCIMPType
objCMethodType :: ObjCSigType (ForeignSig a) => a -> String
objCMethodType thing = ret ++ concat (last args : ":" : init args)
    where
        proxy = (const Nothing :: a -> Maybe (ForeignSig a)) thing
        ret  = retTypeString  proxy
        args = argTypeStrings proxy

objCTypeString :: ObjCType a => a -> String
objCTypeString = typeString . (const Nothing :: a -> Maybe a)
