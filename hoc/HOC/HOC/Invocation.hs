{-# LANGUAGE FlexibleContexts #-}
module HOC.Invocation where

import Foreign
import Foreign.C            ( CInt )
import Foreign.LibFFI.Experimental
import Control.Monad        ( when )

import HOC.CBits
import HOC.Arguments

import HOC.Exception

callWithException cif fun ret args = do
    exception <- c_callWithExceptions (CIF cif) fun ret args
    when (exception /= nullPtr) $
        exceptionObjCToHaskell exception

callWithoutRetval :: SomeCIF -> FunPtr a
                  -> Ptr (Ptr ())
                  -> IO ()

callWithoutRetval cif fun args = callWithException cif fun nullPtr args


callWithRetval :: (ObjCArgument ret, RetType (ForeignArg ret))
               => SomeCIF -> FunPtr a
               -> Ptr (Ptr ())
               -> IO ret

callWithRetval cif fun args =
    withInRet objcInRet (\retptr -> callWithException cif fun retptr args)

getMarshalledArgument :: (ObjCArgument a, ArgType (ForeignArg a)) => Ptr (Ptr ()) -> Int -> IO a
getMarshalledArgument args idx = do
    p <- peekElemOff args idx
    peekArg objcInArg (castPtr p)


kHOCEnteredHaskell = 1 :: CInt
kHOCImportedArguments = 2 :: CInt
kHOCAboutToExportResult = 3 :: CInt
kHOCAboutToLeaveHaskell = 4 :: CInt


