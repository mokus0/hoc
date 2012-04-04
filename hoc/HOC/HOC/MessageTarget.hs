{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
module HOC.MessageTarget where

import Control.Monad                ( when )
import Foreign.LibFFI.Experimental  ( CIF, RetType )
import Foreign.ObjC                 ( SEL )
import Foreign.Ptr                  ( Ptr, nullPtr )
import HOC.Arguments                ( ObjCArgument(..) )
import HOC.CBits                    ( ObjCObject, ID(..), releaseObject )
import HOC.ID                       ( nil )
import HOC.MsgSend                  ( objSendMessageWithRetval, objSendMessageWithoutRetval )

class (ObjCArgument a, ForeignArg a ~ Ptr ObjCObject) => MessageTarget a where
    isNil :: a -> Bool
    
    sendMessageWithRetval :: (ObjCArgument ret, RetType (ForeignArg ret))
                          => a
                          -> CIF (Ptr ObjCObject -> SEL -> b)
                          -> Ptr (Ptr ())
                          -> IO ret

    sendMessageWithoutRetval :: a
                             -> CIF (Ptr ObjCObject -> SEL -> b)
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
    toID (ID a) = ID a
    toID Nil = Nil
    
    fromID (ID a) = ID a
    fromID Nil = Nil

releaseExtraReference :: MessageTarget a => a -> IO a
releaseExtraReference obj
    = withExportedArgument obj (\ptr -> when (ptr /= nullPtr) (releaseObject ptr))
      >> return obj

