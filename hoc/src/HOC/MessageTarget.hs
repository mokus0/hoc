{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
module HOC.MessageTarget where

import Foreign.LibFFI.Experimental  ( SigType, Dynamic, Dyn, dyn )
import Foreign.ObjC                 ( ObjCObject, SEL, releaseObject, msgSendWith )
import Foreign.Ptr                  ( Ptr )
import HOC.Arguments                ( ObjCArgument(..), objcOutArg )
import HOC.ID                       ( ID(..), nil, castObject )

class ObjCArgument a => MessageTarget a where
    isNil :: a -> Bool
    
    sendMessageWith :: SigType b => Dyn b c -> a -> SEL b -> c

sendMessage :: (MessageTarget a, Dynamic b) => a -> SEL b -> b
sendMessage = sendMessageWith dyn

class
    ( MessageTarget a
    , ObjCArgument (CovariantReturn a)
    , Object (CovariantReturn a)
    ) => CovariantReturnTarget a where
    type CovariantReturn a
    type CovariantReturn a = a

class (MessageTarget a, ForeignArg a ~ Ptr ObjCObject) => Object a where
    toID :: a -> ID ()
    fromID :: ID () -> a

instance MessageTarget (ID a) where
    isNil x = x == nil
    
    sendMessageWith = msgSendWith objcOutArg

instance CovariantReturnTarget (ID a)

instance Object (ID a) where
    toID   = castObject
    fromID = castObject

-- called when importing 'Inited' objects to offset the extra
-- retain such objects have.
releaseExtraReference :: Object a => a -> IO a
releaseExtraReference obj = do
    withExportedArgument (toID obj) releaseObject
    return obj

