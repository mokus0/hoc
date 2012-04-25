{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies,
             UndecidableInstances, FlexibleInstances,
             FlexibleContexts, TypeFamilies #-}
module HOC.Super(
        SuperClass, SuperTarget, Super(super), castSuper
    ) where

import Foreign.LibFFI.Experimental ( outByRef )
import Foreign.ObjC             ( ObjCClass, ObjCSuper(..), msgSendSuperWith )
import Foreign.Ptr              ( Ptr, nullPtr )
import HOC.Arguments            ( ObjCArgument(..), objcOutArg )
import HOC.Class                ( RawStaticClass, rawStaticClassForObject )
import HOC.ID                   ( ID, castObject )
import HOC.MessageTarget        ( Object(..), MessageTarget(..) )

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

instance Object a
        => ObjCArgument (SuperTarget a) where
    type ForeignArg (SuperTarget a) = ObjCSuper
    
    withExportedArgument (SuperTarget obj cls) action = 
        withExportedArgument obj $ \obj' -> 
            action (ObjCSuper obj' cls)
    
    exportArgument (SuperTarget obj cls) = do
        obj' <- exportArgument (toID obj)
        return (ObjCSuper obj' cls)
    exportArgumentRetained (SuperTarget obj cls) = do
        obj' <- exportArgumentRetained (toID obj)
        return (ObjCSuper obj' cls)
    importArgument (ObjCSuper obj cls) = do
        obj' <- importArgument obj
        return (SuperTarget (fromID obj') cls)

castSuper :: SuperClass (ID sub) (ID super) => ID sub -> ID super
castSuper = castObject

instance (Object (ID sub), Object (ID super), SuperClass (ID sub) (ID super), 
          RawStaticClass (ID super))
    => Super (ID sub) (SuperTarget (ID super)) where
    super obj = SuperTarget (castSuper obj)
                    (rawStaticClassForObject (castSuper obj)) 

instance Object a => MessageTarget (SuperTarget a) where
    isNil (SuperTarget x cls) = isNil x || cls == nullPtr
    
    sendMessage = msgSendSuperWith (outByRef objcOutArg)
