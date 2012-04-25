{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, UndecidableInstances,
    ScopedTypeVariables, TypeFamilies #-}
module HOC.NewlyAllocated where

{-
    Sending retain and release to objects that have been alloced, but not inited,
    doesn't work in all cases: Sometimes, an init method might decide to
    dealloc the original object --- if the finalizer in ID.hs releases it afterwards,
    we'll crash.
    
    So we introduce a special type for newly allocated objects --- basically
    just an unmanaged pointer. It should only live for a very short time between
    the call to alloc and the call to init.
-}

import Foreign.LibFFI.Experimental ( outByRef )
import Foreign.ObjC     ( ObjCObject, ObjCSuper(..), msgSend, msgSendSuperWith )
import Foreign.Ptr      ( Ptr, castPtr, nullPtr )
import HOC.Arguments    ( ObjCArgument(..), objcOutArg )
import HOC.Class        ( Class, ClassObject(classObject) )
import HOC.ID           ( ID, castObject, importClass )
import HOC.MessageTarget( MessageTarget(..) )
import HOC.Super        ( Super(..), SuperClass )

newtype NewlyAllocated a
    = NewlyAllocated (Ptr ObjCObject)

data NewSuper a
    = NewSuper (Ptr ObjCObject) (Class ())

instance ObjCArgument (NewlyAllocated a) where
    type ForeignArg (NewlyAllocated a) = Ptr ObjCObject
    
    exportArgument (NewlyAllocated p) = return p
    importArgument = return . NewlyAllocated

instance ObjCArgument (NewSuper a) where
    type ForeignArg (NewSuper a) = ObjCSuper
    
    exportArgument (NewSuper obj cls) = do
        cls' <- exportArgument cls
        return (ObjCSuper obj (castPtr cls'))
    
    importArgument (ObjCSuper obj cls) = do
        cls' <- importClass (castPtr cls)
        return (NewSuper obj cls')

-- Note that NewlyAllocated is not an instance of Object. Objects can be converted
-- to IDs, and IDs are reference counted. Not retaining and releasing objects before
-- they have been inited is the whole point of NewlyAllocated (besides some added type
-- safety)..
    
instance MessageTarget (NewlyAllocated a) where
    isNil       (NewlyAllocated obj) = obj == nullPtr
    sendMessage (NewlyAllocated obj) = msgSend obj

instance MessageTarget (NewSuper a) where
    isNil       (NewSuper obj cls) = (obj == nullPtr) || isNil cls
    sendMessage = msgSendSuperWith (outByRef objcOutArg)

instance (SuperClass sub (ID super), ClassObject (Class super))
    => Super (NewlyAllocated sub) (NewSuper (ID super)) where
    super (NewlyAllocated x) = NewSuper x (castObject (classObject :: Class super))
