{-# LANGUAGE DeriveDataTypeable, 
             ScopedTypeVariables #-}
module HOC.Exception where

import Data.Typeable
import Foreign
import Foreign.C.String     ( withCString )
import Prelude              hiding ( catch )
import Control.Exception

import HOC.CBits
import HOC.Arguments
import HOC.ID ({- instances -})

data WrappedNSException = WrappedNSException (ID ())
    deriving Typeable

exceptionObjCToHaskell :: Ptr ObjCObject -> IO a

-- get the exception pointer figure out if it is a NSException
-- or a haskell exception and throw it.
exceptionObjCToHaskell exception = do
    sptr <- unwrapHaskellException exception
    if (castStablePtrToPtr sptr == nullPtr)
        then do
            exc <- importArgument exception
            throwIO $ WrappedNSException exc
        else do
            exc <- deRefStablePtr sptr
            throwIO exc

exceptionHaskellToObjC :: IO a -> IO (Ptr ObjCObject)

exceptionHaskellToObjC action = 
    (action >> return nullPtr)
        `catches` [
            Handler $ \(WrappedNSException exc) -> exportArgument exc,
            Handler $ \(exc :: SomeException) -> withCString (show exc) $
                \cstr -> newStablePtr exc >>= wrapHaskellException cstr
        ]
        
instance Exception WrappedNSException where
    toException = SomeException
    fromException (SomeException ex) = cast ex
        
instance Show WrappedNSException where
    show (WrappedNSException ex) = "<<NSException>>"
        
catchWrappedNSException :: IO a -> (WrappedNSException -> IO a) -> IO a
catchWrappedNSException = catch
