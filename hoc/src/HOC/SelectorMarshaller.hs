{-# LANGUAGE MagicHash, TemplateHaskell #-}
module HOC.SelectorMarshaller(
        SelectorInfo(..),
        mkSelectorInfo,
        mkSelectorInfoRetained,
        makeMarshaller,
        makeMarshallers,
        marshallerName
    ) where

import Foreign.ObjC                 ( SEL, getSEL )
import GHC.Base                     ( unpackCString# )
import HOC.Arguments                ( importArgument, withExportedArgument )
import HOC.MessageTarget
import Language.Haskell.TH
import System.IO.Unsafe             ( unsafePerformIO )

data SelectorInfo a = SelectorInfo {
        selectorInfoObjCName :: String,
        selectorInfoHaskellName :: String,
        selectorInfoSel :: !(SEL a),
        selectorInfoResultRetained :: !Bool
    }

{-# NOINLINE mkSelectorInfo #-}
mkSelectorInfo objCName hsName
    = SelectorInfo objCName hsName (getSEL objCName) False

{-# NOINLINE mkSelectorInfo# #-}
mkSelectorInfo# objCName# hsName#
    -- NOTE: Don't call mkSelectorInfo here, the rule would apply!
    = SelectorInfo objCName hsName (getSEL objCName) False
    where
        objCName = unpackCString# objCName#
        hsName   = unpackCString# hsName#

{-# RULES
"litstr" forall s1 s2.
    mkSelectorInfo (unpackCString# s1) (unpackCString# s2)
    = mkSelectorInfo# s1 s2
  #-}

{-# NOINLINE mkSelectorInfoRetained #-}
mkSelectorInfoRetained objCName hsName
    = SelectorInfo objCName hsName (getSEL objCName) True

{-# NOINLINE mkSelectorInfoRetained# #-}
mkSelectorInfoRetained# objCName# hsName#
    -- NOTE: Don't call mkSelectorInfo here, the rule would apply!
    = SelectorInfo objCName hsName (getSEL objCName) True
    where
        objCName = unpackCString# objCName#
        hsName   = unpackCString# hsName#

{-# RULES
"litstr" forall s1 s2.
    mkSelectorInfoRetained (unpackCString# s1) (unpackCString# s2)
    = mkSelectorInfoRetained# s1 s2
  #-}


makeMarshaller maybeInfoName haskellName nArgs isUnit isPure isRetained =
            funD haskellName [
                clause (map varP $ infoArgument ++ map mkName arguments
                        ++ [mkName "target"])
                     (normalB $ marshallerBody
                    ) []
            ]
    where
        (infoVar, infoArgument) = case maybeInfoName of
                    Just name -> (varE name, [])
                    Nothing -> (varE (mkName "info"), [mkName "info"])
        arguments = [ "arg" ++ show i | i <- [1..nArgs] ]
        argumentsToMarshal = map (varE.mkName) arguments
        marshalledArguments = map (mkName . (++"'")) arguments
   
        marshallerBody = purify $
                         checkTargetNil $
                         releaseRetvalIfRetained $
                         marshallArgs  $
                         marshallReturn $
                         invoke
        
        marshallArgs = marshallArgs' argumentsToMarshal marshalledArguments
            where
                marshallArgs' [] [] e = e
                marshallArgs' (arg:args) (arg':args') e =
                    [| withExportedArgument $arg $(lamE [varP arg'] e') |]
                    where e' = marshallArgs' args args' e
        
        invoke = appsE
                ( [| sendMessage $targetVar (selectorInfoSel $infoVar) |] 
                : map varE marshalledArguments)
            where
                targetVar = varE (mkName "target")
        
        marshallReturn e = [| $e >>= importArgument |]
        
        purify e | isPure = [| unsafePerformIO $(e) |]
                 | otherwise = e
                 
        releaseRetvalIfRetained e | isRetained = [| $e >>= releaseExtraReference |]
                                  | otherwise = e
                                  
        checkTargetNil e = [| failNilMessage $(varE $ mkName "target")
                                             (selectorInfoHaskellName $(infoVar))
                              >> $(e) |]
    
makeMarshallers n =
        sequence $
        [ makeMarshaller Nothing (mkName $ marshallerName nArgs isUnit) nArgs isUnit False False
        | nArgs <- [0..n], isUnit <- [False, True] ]

marshallerName nArgs False = "method" ++ show nArgs
marshallerName nArgs True = "method" ++ show nArgs ++ "_"

failNilMessage :: MessageTarget t => t -> String -> IO ()
failNilMessage target selectorName
    | isNil target = fail $ "Message sent to nil: " ++ selectorName
    | otherwise = return ()
