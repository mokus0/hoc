{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
module HOC.MsgSend(
        objSendMessageWithRetval,
        objSendMessageWithoutRetval,
        superSendMessageWithRetval,
        superSendMessageWithoutRetval
    ) where

import Foreign.LibFFI.Experimental  ( CIF, RetType )
import Foreign.ObjC                 ( ObjCObject, SEL )
import Foreign.Ptr                  ( Ptr )
import HOC.Arguments                ( ObjCArgument, ForeignArg )
import HOC.CBits
import HOC.Invocation               ( callWithRetval, callWithoutRetval )

#ifdef GNUSTEP

import Foreign.Ptr                  ( castPtr )
import Foreign.Storable             ( peek, poke, peekElemOff )

#endif

objSendMessageWithRetval
    :: (ObjCArgument ret, RetType (ForeignArg ret))
    => CIF (Ptr ObjCObject -> SEL a -> a)
    -> Ptr (Ptr ())
    -> IO ret

objSendMessageWithoutRetval
    :: CIF (Ptr ObjCObject -> SEL a -> a)
    -> Ptr (Ptr ())
    -> IO ()

superSendMessageWithRetval
    :: (ObjCArgument ret, RetType (ForeignArg ret))
    => CIF (Ptr ObjCObject -> SEL a -> a)
    -> Ptr (Ptr ())
    -> IO ret

superSendMessageWithoutRetval
    :: CIF (Ptr ObjCObject -> SEL a -> a)
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
