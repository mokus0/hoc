{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
module HOC.CStruct( declareCStruct, declareCStructWithTag ) where
    
import HOC.Arguments        ( ObjCArgument(..) )
import HOC.TH
import HOC.NameCaseChange   ( nameToUppercase )

import Control.Monad.State
import Data.Maybe           ( fromMaybe )
import Foreign
import Foreign.LibFFI.Experimental
import Foreign.ObjC

declareCStruct :: String -> [TypeQ] -> Q [Dec]
declareCStructWithTag :: String -> Maybe String -> [TypeQ] -> Q [Dec]


mkRawThing :: ObjCArgument a => a -> ForeignArg a
mkRawThing _ = undefined

sizeMember :: (ObjCArgument a, Storable (ForeignArg a)) => a -> State Int ()
sizeMember thing =
    modify (\offset -> align offset (alignment rawThing) + sizeOf rawThing)

    where align x a = (x + (a-1)) .&. complement (a-1)
          rawThing = mkRawThing thing

alignMember :: (ObjCArgument a, Storable (ForeignArg a)) => a -> Int
alignMember = alignment . mkRawThing

pokeMember :: (ObjCArgument a, Storable (ForeignArg a)) => a -> StateT (Ptr c) IO ()
pokeMember thing = do
    rawThing <- lift $ exportArgument thing
    modify (`alignPtr` alignment rawThing)
    p <- get
    lift $ poke (castPtr p) rawThing
    modify (`plusPtr` sizeOf rawThing)

peekMember :: (ObjCArgument a, Storable (ForeignArg a)) => StateT (Ptr c) IO a
peekMember = (mfix $ \result -> do
    modify (`alignPtr` alignment result)
    p <- get
    rawThing <- lift $ peek (castPtr p)
    modify (`plusPtr` sizeOf rawThing)
    return rawThing) >>= \rawThing -> lift (importArgument rawThing)
    
ffiMember :: ObjCArgument a => a -> State [SomeType] ()
ffiMember thing = modify (ffiTypeOf_ (foreign thing) :)
    where foreign = const Nothing :: a -> Maybe (ForeignArg a)

memberTypeString :: ObjCType a => a -> String
memberTypeString = typeString . (const Nothing :: a -> Maybe a)

declareCStruct cname fieldTypes
    = declareCStructWithTag cname Nothing fieldTypes

declareCStructWithTag cname mbTag fieldTypes
    = do
        let name = mkName $ nameToUppercase cname
            structTag = fromMaybe "?" mbTag
        dataDecl <- dataD (cxt []) name [] [
                normalC name $
                    map (strictType (return NotStrict)) fieldTypes
            ] [''Eq, ''Ord] --, ''Read, ''Show]
        
        varNames <- mapM (const $ newName "field") fieldTypes
        ptrName <- newName "ptr"
    
        let takeApartP = conP name $ map varP varNames
            putTogetherE | null varNames = conE name
                         | otherwise = appsE $ (conE name : map varE varNames)

            doWithArgs name | null varNames = [| return () |]
            doWithArgs name = doE $
                [ noBindS (varE name `appE` varE field)
                | field <- varNames ]
                
            doWithResults name = doE $
                [ bindS (varP field) (varE name)
                | field <- varNames ]
                ++ [ noBindS [| return $(putTogetherE) |] ]

            mapArgs name = listE $
                [ varE name `appE` varE field | field <- varNames ]

        storableDecl <- instanceD (cxt []) (conT ''Storable `appT` conT name)
            [
                funD 'alignment [
                    clause [tildeP takeApartP]
                        (normalB [| maximum ( 1 : $(mapArgs 'alignMember) ) |]) []
                ],
                funD 'sizeOf [
                    clause [tildeP takeApartP] 
                        (normalB [| execState $(doWithArgs 'sizeMember) 0 |])
                        []
                ],
                funD 'poke [
                    clause [varP ptrName, takeApartP] 
                        (normalB [| evalStateT $(doWithArgs 'pokeMember) $(varE ptrName) |])
                        []
                ],
                funD 'peek [
                    clause [varP ptrName]
                        (normalB [| evalStateT $(doWithResults 'peekMember) $(varE ptrName) |])
                        []
                ]
            ]
    
        ffiDecls <- [d|
                    instance FFIType $(conT name) where
                        ffiType = Type 
                            $( caseE [| undefined |] 
                                [ match (tildeP takeApartP)
                                    (normalB [| struct . reverse
                                                    $ execState $(doWithArgs 'ffiMember) []
                                             |])
                                    []
                                ]
                            )
                    instance ObjCType $(conT name) where
                        typeString p = 
                            $( caseE [| (undefined :: p a -> a) p |] 
                                [ match (tildeP takeApartP)
                                    (normalB [| "{" ++ structTag ++ "=" ++ 
                                                concat $(mapArgs 'memberTypeString) ++
                                                "}" |])
                                    []
                                ]
                            )
                    instance ArgType $(conT name)
                    instance RetType $(conT name)
                    instance ObjCArg $(conT name)
                    instance ObjCRet $(conT name)
               |]
    
        argDecl <- instanceD (cxt []) (conT ''ObjCArgument `appT` conT name) []
        
        return (dataDecl : storableDecl : argDecl : ffiDecls)
