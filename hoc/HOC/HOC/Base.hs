module HOC.Base where

import Control.Exception(bracket)
import HOC.CBits

withAutoreleasePool :: IO a -> IO a
withAutoreleasePool action = bracket newAutoreleasePool releaseObject (const action)
