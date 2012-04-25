{-# LANGUAGE TemplateHaskell #-}
module HOC.ExternFunctions(declareExternFun) where

import Foreign.LibFFI.Experimental  ( importDynWithCall, cif, CIF, dyn )
import Foreign.ObjC                 ( objc_ffi_call )
import HOC.Arguments                ( ForeignSig, withExportedArgument, importArgument )
import HOC.Dyld                     ( lookupSymbol )
import HOC.NameCaseChange           ( nameToLowercase )
import HOC.TH                       ( countArgs )
import Language.Haskell.TH hiding (dyn)
import System.IO.Unsafe             ( unsafePerformIO )

declareExternFun :: String -> TypeQ -> Q [Dec]
declareExternFun name typeSigQ = do
    typeSig <- typeSigQ
    
    let n = mkName $ nameToLowercase name
        cifE = [| cif :: CIF (ForeignSig $typeSigQ) |]
        ptrN = mkName $ "c__" ++ name
        
        -- ### FIXME: Code Duplication from SelectorMarshaller.hs
        arguments = [ "arg" ++ show i | i <- [1..countArgs typeSig] ]
        
        argumentsToMarshal = map (varE.mkName) arguments
        marshalledArguments = map (mkName . (++"'")) arguments
        
        marshallerBody = marshallArgs   $
                         marshallReturn $
                         invoke
        
        marshallArgs = marshallArgs' argumentsToMarshal marshalledArguments
            where
                marshallArgs' [] [] e = e
                marshallArgs' (arg:args) (arg':args') e =
                    [| withExportedArgument $arg $(lamE [varP arg'] e') |]
                    where e' = marshallArgs' args args' e
        
        marshallReturn e = [| $e >>= importArgument |]
        
        invoke = appsE
            ( [| importDynWithCall (objc_ffi_call $cifE) dyn $(varE ptrN) |]
            : map varE marshalledArguments)
    
    sequence [
            sigD n typeSigQ,
            funD n [
                clause (map (varP.mkName) arguments)
                       (normalB $ marshallerBody)
                       [ valD (varP ptrN) (normalB
                            [| unsafePerformIO $ lookupSymbol $(stringE name) 
                             |]) [] ]
            ]
        ]
