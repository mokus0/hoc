{-# LANGUAGE FlexibleContexts #-}
module HOC.Invocation where

import Control.Monad                ( when )
import Foreign.Ptr                  ( Ptr, castPtr, nullPtr, FunPtr )
import Foreign.Storable             ( peekElemOff )
import Foreign.C                    ( CInt )
import Foreign.LibFFI.Experimental  ( CIF, RetType, withInRet, ArgType, peekArg )
import HOC.Arguments                ( ObjCArgument, ForeignArg, objcInRet, objcInArg )
import HOC.CBits                    ( c_callWithExceptions )
import HOC.Exception                ( exceptionObjCToHaskell )

callWithException cif fun ret args = do
    exception <- c_callWithExceptions cif fun ret args
    when (exception /= nullPtr) $
        exceptionObjCToHaskell exception

callWithoutRetval :: CIF a -> FunPtr a
                  -> Ptr (Ptr ())
                  -> IO ()

callWithoutRetval cif fun args = callWithException cif fun nullPtr args


callWithRetval :: (ObjCArgument ret, RetType (ForeignArg ret))
               => CIF a -> FunPtr a
               -> Ptr (Ptr ())
               -> IO ret

callWithRetval cif fun args =
    withInRet objcInRet (\retptr -> callWithException cif fun retptr args)

getMarshalledArgument :: (ObjCArgument a, ArgType (ForeignArg a)) => Ptr (Ptr ()) -> Int -> IO a
getMarshalledArgument args idx = do
    p <- peekElemOff args idx
    peekArg objcInArg (castPtr p)


kHOCEnteredHaskell      = 1 :: CInt
kHOCImportedArguments   = 2 :: CInt
kHOCAboutToExportResult = 3 :: CInt
kHOCAboutToLeaveHaskell = 4 :: CInt


