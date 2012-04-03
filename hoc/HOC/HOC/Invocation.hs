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

withMarshalledArgument :: (ObjCArgument a, Storable (ForeignArg a)) => a -> (Ptr () -> IO c) -> IO c

withMarshalledArgument arg act = 
    withExportedArgument arg (\exported -> with exported (act . castPtr))

callWithoutRetval :: SomeCIF -> FunPtr a
                  -> Ptr (Ptr ())
                  -> IO ()

callWithoutRetval cif fun args = callWithException cif fun nullPtr args


callWithRetval :: ObjCArgument ret
               => SomeCIF -> FunPtr a
               -> Ptr (Ptr ())
               -> IO ret

callWithRetval cif fun args =
    withInRet inRet (\retptr -> callWithException cif fun retptr args)
        >>= importArgument


setMarshalledRetval :: ObjCArgument a => Bool -> Ptr () -> a -> IO ()
setMarshalledRetval retained ptr val = withOutRet outRet
    ((if retained then exportArgumentRetained else exportArgument) val)
    (castPtr ptr)

getMarshalledArgument :: (ObjCArgument a, Storable (ForeignArg a)) => Ptr (Ptr ()) -> Int -> IO a
getMarshalledArgument args idx = do
    p <- peekElemOff args idx
    arg <- peek (castPtr p)
    importArgument arg


kHOCEnteredHaskell = 1 :: CInt
kHOCImportedArguments = 2 :: CInt
kHOCAboutToExportResult = 3 :: CInt
kHOCAboutToLeaveHaskell = 4 :: CInt


