{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module HOC.CBits.MsgSend where

import Foreign.ObjC
import Foreign.Ptr

#ifdef GNUSTEP

foreign import ccall "objc/objc.h objc_msg_lookup"
    objc_msg_lookup :: Ptr ObjCObject -> SEL a -> IO (FunPtr (Ptr ObjCObject -> SEL a -> a))

foreign import ccall "objc/objc.h objc_msg_lookup_super"
    objc_msg_lookup_super :: Ptr ObjCObject -> SEL a -> IO (FunPtr (Ptr ObjCObject -> SEL a -> a))
    
#else
    
    -- the type signatures are essentially bogus
    -- the return value is not necessarily (), and might even be a struct.
    -- we only call them via libffi, so we couldn't care less.
foreign import ccall "MsgSend.h &objc_msgSend"
    objc_msgSendPtr :: FunPtr (Ptr ObjCObject -> SEL a -> a)
foreign import ccall "MsgSend.h &objc_msgSend_stret"
    objc_msgSend_stretPtr :: FunPtr (Ptr ObjCObject -> SEL a -> a)

foreign import ccall "MsgSend.h &objc_msgSendSuper"
    objc_msgSendSuperPtr :: FunPtr (Ptr ObjCObject -> SEL a -> a)
foreign import ccall "MsgSend.h &objc_msgSendSuper_stret"
    objc_msgSendSuper_stretPtr :: FunPtr (Ptr ObjCObject -> SEL a -> a)

#endif