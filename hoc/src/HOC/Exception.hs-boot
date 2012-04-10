module HOC.Exception where

import Foreign.Ptr
import HOC.CBits.Types

exceptionObjCToHaskell :: Ptr ObjCObject -> IO a
exceptionHaskellToObjC :: IO a -> IO (Ptr ObjCObject)
