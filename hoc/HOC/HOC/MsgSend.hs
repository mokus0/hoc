{-# LANGUAGE CPP #-}
module HOC.MsgSend(
        objSendMessageWithRetval,
        objSendMessageWithoutRetval,
        superSendMessageWithRetval,
        superSendMessageWithoutRetval
    ) where

import HOC.Base
import HOC.CBits
import HOC.FFICallInterface
import HOC.Arguments
import HOC.Invocation
import Foreign

objSendMessageWithRetval
    :: ObjCArgument a b
    => FFICif
    -> Ptr (Ptr ())
    -> IO a

objSendMessageWithoutRetval
    :: FFICif
    -> Ptr (Ptr ())
    -> IO ()

superSendMessageWithRetval
    :: ObjCArgument a b
    => FFICif
    -> Ptr (Ptr ())
    -> IO a

superSendMessageWithoutRetval
    :: FFICif
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

withMarshalledDummy :: ObjCArgument a b => (b -> IO a) -> IO a
withMarshalledDummy action = action undefined

objSendMessageWithRetval cif args =
    withMarshalledDummy $ \dummy ->
    cifIsStret cif >>= \isStret ->
    callWithRetval cif (if isStret /= 0
                                then objc_msgSend_stretPtr
                                else objc_msgSendPtr) args       

objSendMessageWithoutRetval cif args =
    callWithoutRetval cif objc_msgSendPtr args


superSendMessageWithRetval cif args =
    withMarshalledDummy $ \dummy ->
    cifIsStret cif >>= \isStret ->
    callWithRetval cif (if isStret /= 0
                                then objc_msgSendSuper_stretPtr
                                else objc_msgSendSuperPtr) args      

superSendMessageWithoutRetval cif args =
    callWithoutRetval cif objc_msgSendSuperPtr args

#endif
