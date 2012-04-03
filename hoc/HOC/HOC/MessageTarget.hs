{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
module HOC.MessageTarget where

import Control.Monad
import HOC.CBits
import HOC.Arguments
import HOC.ID
import HOC.MsgSend
import Foreign.LibFFI.Experimental
import Foreign.ObjC
import Foreign.Ptr

class (ObjCArgument a, ForeignArg a ~ Ptr ObjCObject) => MessageTarget a where
    isNil :: a -> Bool
    
    sendMessageWithRetval :: ObjCArgument ret
                          => a
                          -> SomeCIF
                          -> Ptr (Ptr ())
                          -> IO ret

    sendMessageWithoutRetval :: a
                             -> SomeCIF
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

failNilMessage :: MessageTarget t => t -> String -> IO ()
failNilMessage target selectorName
    | isNil target = fail $ "Message sent to nil: " ++ selectorName
    | otherwise = return ()

releaseExtraReference :: MessageTarget a => a -> IO a
releaseExtraReference obj
    = withExportedArgument obj (\ptr -> when (ptr /= nullPtr) (releaseObject ptr))
      >> return obj

