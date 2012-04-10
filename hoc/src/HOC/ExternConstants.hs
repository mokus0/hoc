{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
module HOC.ExternConstants(declareExternConst) where

import Foreign.Ptr          ( castFunPtrToPtr )
import Foreign.Storable     ( Storable(peek) )
import HOC.Arguments        ( ObjCArgument(importArgument), ForeignArg )
import HOC.Dyld             ( lookupSymbol )
import HOC.NameCaseChange   ( nameToLowercase )
import HOC.TH
import System.IO.Unsafe     ( unsafePerformIO )

declareExternConst :: String -> TypeQ -> Q [Dec]
declareExternConst name typ
    = sequence [
        sigD n typ,
        valD (varP n) (normalB expr) []
    ]
    where
        n = mkName $ nameToLowercase name
        expr = [| getGlobalVar $(stringE name) |]

getGlobalVar :: (ObjCArgument a, Storable (ForeignArg a)) => String -> a
getGlobalVar name = unsafePerformIO $
    lookupSymbol name
    >>= peek . castFunPtrToPtr
    >>= importArgument
