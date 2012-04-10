module HOC.Dyld ( lookupSymbol ) where

import Foreign.Ptr                  ( FunPtr )
import System.Posix.DynamicLinker   ( dlsym, DL(Default) )

lookupSymbol :: String -> IO (FunPtr a)
lookupSymbol = dlsym Default

