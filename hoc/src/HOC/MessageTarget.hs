{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
module HOC.MessageTarget where

import Control.Monad                ( when )
import Foreign.LibFFI.Experimental  ( CIF, RetType )
import Foreign.ObjC                 ( ObjCObject, SEL, releaseObject )
import Foreign.Ptr                  ( Ptr, nullPtr )
import HOC.Arguments                ( ObjCArgument(..) )
import HOC.ID                       ( ID(..), nil, castObject )
import HOC.MsgSend                  ( objSendMessageWithRetval, objSendMessageWithoutRetval )

class (ObjCArgument a, ForeignArg a ~ Ptr ObjCObject) => MessageTarget a where
    isNil :: a -> Bool
    
    sendMessageWithRetval :: (ObjCArgument ret, RetType (ForeignArg ret))
                          => a
                          -> CIF (Ptr ObjCObject -> SEL b -> b)
                          -> Ptr (Ptr ())
                          -> IO ret

    sendMessageWithoutRetval :: a
                             -> CIF (Ptr ObjCObject -> SEL b -> b)
                             -> Ptr (Ptr ())
                             -> IO ()

class MessageTarget a => Object a where
    toID :: a -> ID ()
    fromID :: ID () -> a

instance MessageTarget (ID a) where
    isNil x = x == nil
    
    sendMessageWithRetval _ = objSendMessageWithRetval
    sendMessageWithoutRetval _ = objSendMessageWithoutRetval

instance Object (ID a) where
    toID   = castObject
    fromID = castObject

-- called when importing 'Inited' objects to offset the extra
-- retain such objects have.
releaseExtraReference :: MessageTarget a => a -> IO a
releaseExtraReference obj
    = withExportedArgument obj (\ptr -> when (ptr /= nullPtr) (releaseObject ptr))
      >> return obj

