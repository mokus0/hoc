{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
module HOC.Invocation where

import Foreign.LibFFI.Experimental  ( CIF, RetType, withInRet, ArgType, peekArg )
import Foreign.ObjC                 ( objc_ffi_call )
import Foreign.Ptr                  ( Ptr, castPtr, nullPtr, FunPtr )
import Foreign.Storable             ( peekElemOff )
import HOC.Arguments                ( ObjCArgument, ForeignArg, objcInRet, objcInArg )

callWithoutRetval :: CIF a -> FunPtr a
                  -> Ptr (Ptr ())
                  -> IO ()

callWithoutRetval cif fun args = objc_ffi_call cif fun nullPtr args


callWithRetval :: (ObjCArgument ret, RetType (ForeignArg ret))
               => CIF a -> FunPtr a
               -> Ptr (Ptr ())
               -> IO ret

callWithRetval cif fun args =
    withInRet objcInRet (\retptr -> objc_ffi_call cif fun (castPtr retptr) args)

getMarshalledArgument :: (ObjCArgument a, ArgType (ForeignArg a)) => Ptr (Ptr ()) -> Int -> IO a
getMarshalledArgument args idx = do
    p <- peekElemOff args idx
    peekArg objcInArg (castPtr p)
