{-# LANGUAGE TemplateHaskell, EmptyDataDecls, TypeFamilies,
             FlexibleContexts, ScopedTypeVariables, DefaultSignatures, 
             UndecidableInstances #-}
module HOC.Arguments where

import Foreign.LibFFI.Experimental
import Foreign.Ptr
import Foreign.Storable

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

class SigType (ForeignSig t) => ObjCSig t where
    objcDyn :: Dyn (ForeignSig t) t

instance (ObjCArgument a, RetType (ForeignArg a)) => ObjCSig (IO a) where
    objcDyn = mkDyn objcInRet

instance (ObjCArgument a, ArgType (ForeignArg a), ObjCSig b) => ObjCSig (a -> b) where
    objcDyn = consDyn objcOutArg objcDyn

type family DropSelTarget a
type instance DropSelTarget (a -> IO b) = IO b
type instance DropSelTarget (a -> b -> c) = a -> DropSelTarget (b -> c)

getMarshalledArgument :: (ObjCArgument a, ArgType (ForeignArg a)) => Ptr (Ptr ()) -> Int -> IO a
getMarshalledArgument args idx = do
    p <- peekElemOff args idx
    peekArg objcInArg (castPtr p)
