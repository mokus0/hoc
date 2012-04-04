module HOC.Base where

import Control.Exception(bracket)
import Foreign.C.String (withCString)
import Foreign.ObjC
import HOC.CBits
import System.IO.Unsafe (unsafePerformIO)

-- The SEL Type

getSelectorForName :: String -> SEL
getSelectorForName str = unsafePerformIO $
    withCString str sel_registerName

withAutoreleasePool :: IO a -> IO a
withAutoreleasePool action = bracket newAutoreleasePool releaseObject (const action)
