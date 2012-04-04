{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
module HOC.MsgSend(
        objSendMessageWithRetval,
        objSendMessageWithoutRetval,
        superSendMessageWithRetval,
        superSendMessageWithoutRetval
    ) where

import HOC.CBits
import HOC.Arguments
import HOC.Invocation
import Foreign
import Foreign.LibFFI.Experimental
import Foreign.ObjC

objSendMessageWithRetval
    :: (ObjCArgument ret, RetType (ForeignArg ret))
    => SomeCIF
    -> Ptr (Ptr ())
    -> IO ret

objSendMessageWithoutRetval
    :: SomeCIF
    -> Ptr (Ptr ())
    -> IO ()

superSendMessageWithRetval
    :: (ObjCArgument ret, RetType (ForeignArg ret))
    => SomeCIF
    -> Ptr (Ptr ())
    -> IO ret

superSendMessageWithoutRetval
    :: SomeCIF
    -> Ptr (Ptr ())
    -> IO ()

#ifdef GNUSTEP

sndMsgCommon call cif args = do
    target <- peekElemOff args 0 >>= peek . castPtr
    selector <- peekElemOff args 1 >>= peek . castPtr
    imp <- objc_msg_lookup target selector
    call cif imp args
    
sndMsgSuperCommon call cif args = do
    arg0Ptr <- peekElemOff args 0
    super <- peek (castPtr arg0Ptr)
    object <- peek (castPtr super)
    poke (castPtr arg0Ptr) (object :: Ptr ObjCObject)
    
    selector <- peekElemOff args 1 >>= peek . castPtr
    
    imp <- objc_msg_lookup_super super selector
    
    call cif imp args

objSendMessageWithRetval      = sndMsgCommon      callWithRetval
objSendMessageWithoutRetval   = sndMsgCommon      callWithoutRetval
superSendMessageWithRetval    = sndMsgSuperCommon callWithRetval
superSendMessageWithoutRetval = sndMsgSuperCommon callWithoutRetval

#else

ifStret cif t f = do
    isStret <- cifIsStret cif
    return $! if isStret /= 0 then t else f

objSendMessageWithRetval cif args = do
    msgSend <- ifStret cif objc_msgSend_stretPtr objc_msgSendPtr
    callWithRetval cif msgSend args

objSendMessageWithoutRetval cif args =
    callWithoutRetval cif objc_msgSendPtr args

superSendMessageWithRetval cif args = do
    msgSend <- ifStret cif objc_msgSendSuper_stretPtr objc_msgSendSuperPtr
    callWithRetval cif msgSend args

superSendMessageWithoutRetval cif args =
    callWithoutRetval cif objc_msgSendSuperPtr args

#endif
