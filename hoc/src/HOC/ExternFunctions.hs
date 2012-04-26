{-# LANGUAGE TemplateHaskell #-}
module HOC.ExternFunctions(declareExternFun) where

import Foreign.LibFFI.Experimental  ( importDynWithCall, cif )
import Foreign.ObjC                 ( objc_ffi_call )
import Foreign.Ptr                  ( FunPtr )
import HOC.Arguments                ( ForeignSig, ObjCSig(..) )
import HOC.Dyld                     ( lookupSymbol )
import HOC.NameCaseChange           ( nameToLowercase )
import Language.Haskell.TH hiding (dyn)
import System.IO.Unsafe             ( unsafePerformIO )

objCCall :: ObjCSig a => FunPtr (ForeignSig a) -> a
objCCall = importDynWithCall (objc_ffi_call cif) objcDyn

declareExternFun :: String -> TypeQ -> Q [Dec]
declareExternFun name typeSigQ = do
    let n = mkName $ nameToLowercase name
        ptrN = mkName $ "c__" ++ name
    
    sequence [
            sigD n typeSigQ,
            valD (varP n) (normalB [| objCCall $(varE ptrN) |])
               [ pragInlD ptrN (inlineSpecNoPhase False False)
               , valD (varP ptrN) (normalB
                    [| unsafePerformIO $ lookupSymbol $(stringE name) 
                     |]) [] ]
        ]
