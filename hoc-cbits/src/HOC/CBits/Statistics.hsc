{-# LANGUAGE ForeignFunctionInterface #-}
module HOC.CBits.Statistics where

#include "Statistics.h"

import Foreign.C.Types
import Foreign.Ptr

newtype HOCEvent = HOCEvent CInt

foreign import ccall unsafe
    recordHOCEvent :: HOCEvent -> Ptr (Ptr ()) -> IO ()

kHOCEnteredHaskell :: HOCEvent
kHOCEnteredHaskell = HOCEvent (#const kHOCEnteredHaskell)

kHOCImportedArguments :: HOCEvent
kHOCImportedArguments = HOCEvent (#const kHOCImportedArguments)

kHOCAboutToExportResult :: HOCEvent
kHOCAboutToExportResult = HOCEvent (#const kHOCAboutToExportResult)

kHOCAboutToLeaveHaskell :: HOCEvent
kHOCAboutToLeaveHaskell = HOCEvent (#const kHOCAboutToLeaveHaskell)
