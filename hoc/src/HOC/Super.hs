{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies,
             UndecidableInstances, FlexibleInstances,
             FlexibleContexts, TypeFamilies #-}
module HOC.Super(
        SuperClass, SuperTarget, Super(super), withExportedSuper, castSuper
    ) where

import Foreign.Marshal.Alloc    ( allocaBytes )
import Foreign.ObjC             ( ObjCClass, ObjCObject )
import Foreign.Ptr              ( Ptr, nullPtr )
import Foreign.Storable         ( pokeByteOff, sizeOf )
import HOC.Arguments            ( ObjCArgument(..) )
import HOC.CBits                ( ID, castObject )
import HOC.Class                ( RawStaticClass, rawStaticClassForObject )
import HOC.MessageTarget        ( Object, MessageTarget(..) )
import HOC.MsgSend

{-
    Messages to super.
        [super foo]
    is written as
        super self # foo
-}

-- This defines the type class for type inheritance.  sub uniquely defines 
-- super, which is sufficient to define a class hierarchy.
class SuperClass sub super | sub -> super

data SuperTarget a = SuperTarget a (Ptr ObjCClass)

class Super sub super | sub -> super where
    super :: sub -> super

--- 

pokeSuper objcSuper obj cls
    = pokeByteOff objcSuper 0 obj >> pokeByteOff objcSuper (sizeOf obj) cls

withExportedSuper p cls action = 
    allocaBytes (sizeOf p + sizeOf cls) $ \sptr ->
    pokeSuper sptr p cls >> action sptr

instance MessageTarget a
        => ObjCArgument (SuperTarget a) where
    type ForeignArg (SuperTarget a) = Ptr ObjCObject

    withExportedArgument (SuperTarget obj cls) action =
        withExportedArgument obj $ \p ->
        withExportedSuper p cls action
        
    exportArgument _ = fail "HOC.Super: exportArgument"
    importArgument _ = fail "HOC.Super: importArgument"

castSuper :: SuperClass (ID sub) (ID super) => ID sub -> ID super
castSuper = castObject

instance (Object (ID sub), Object (ID super), SuperClass (ID sub) (ID super), 
          RawStaticClass (ID super))
    => Super (ID sub) (SuperTarget (ID super)) where
    super obj = SuperTarget (castSuper obj)
                    (rawStaticClassForObject (castSuper obj)) 

instance MessageTarget a => MessageTarget (SuperTarget a) where
    isNil (SuperTarget x cls) = isNil x || cls == nullPtr
    
    sendMessageWithRetval _ = superSendMessageWithRetval
    sendMessageWithoutRetval _ = superSendMessageWithoutRetval
