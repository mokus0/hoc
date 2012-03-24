module HOC.Base where

import Control.Exception(bracket)
import Foreign.C.String (withCString)
import HOC.CBits
import System.IO.Unsafe (unsafePerformIO)

-- The SEL Type

getSelectorForName :: String -> SEL
getSelectorForName str = unsafePerformIO $
    withCString str (c_getSelectorForName)

withAutoreleasePool :: IO a -> IO a
withAutoreleasePool action = bracket newAutoreleasePool releaseObject (const action)
