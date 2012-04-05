{-# LANGUAGE ForeignFunctionInterface #-}
module HOC.CBits.GetNewHaskellData where

import Data.Dynamic
import Foreign.ObjC
import Foreign.Ptr
import Foreign.StablePtr

-- must be "safe", because it calls methods implemented in Haskell.
foreign import ccall safe "GetNewHaskellData.h getNewHaskellData"
    getNewHaskellData :: Ptr ObjCObject -> IO (StablePtr ([Dynamic]))
foreign import ccall safe "GetNewHaskellData.h getNewHaskellDataForClass"
    getNewHaskellDataForClass :: Ptr ObjCObject
                              -> Ptr ObjCObject
                              -> IO (StablePtr ([Dynamic]))

