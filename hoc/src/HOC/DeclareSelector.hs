{-# LANGUAGE TemplateHaskell, EmptyDataDecls #-}
module HOC.DeclareSelector where
    
import Control.Monad            ( MonadPlus(mplus) )
import Data.Maybe               ( fromMaybe )
import Data.List                ( nub )
import HOC.Arguments            ( ForeignSig, DropSelTarget )
import HOC.Class                ( ClassAndObject )
import HOC.ID                   ( ID )
import HOC.MessageTarget        ( MessageTarget, Object, CovariantReturnTarget(..) )
import HOC.NewlyAllocated       ( NewlyAllocated )
import HOC.SelectorMarshaller
import HOC.SelectorNameMangling ( mangleSelectorName )
import HOC.StdArgumentTypes     ( {- instances -} )
import HOC.Super                ( SuperTarget )
import HOC.TH

data Covariant
data CovariantInstance
data Allocated
data Inited
data Retained a

$(makeMarshallers 4)
marshallersUpTo :: Int
marshallersUpTo = 4

{-# NOINLINE method0 #-} 
{-# NOINLINE method0_ #-}
{-# NOINLINE method1 #-}
{-# NOINLINE method1_ #-}
{-# NOINLINE method2 #-}
{-# NOINLINE method2_ #-}
{-# NOINLINE method3 #-}
{-# NOINLINE method3_ #-}
{-# NOINLINE method4 #-}
{-# NOINLINE method4_ #-}

declareRenamedSelector name haskellName typeSigQ =
    do
        typeSig <- typeSigQ
        let
            -- selectorName = "selector_" ++ haskellName
            infoName = "info_" ++ haskellName
            className = "Has_" ++ haskellName
            imptypeName = "ImpType_" ++ haskellName
       
            nArgs = countArgs typeSig :: Int
            
            -- isPure is a boolean that indicates if the resultType of our type
            -- signature is not in the IO mondad
            -- 
            -- pureType is the type that was used to initalize IO
            -- 
            -- all selectors must be in the IO monad.
            (isPure, pureType) = case resultType typeSig of
                (ConT con) `AppT` ty
                    | con == ''IO -> (False, ty)
                    | otherwise -> badType
                _ -> badType
                where
                    badType = error $ haskellName ++ " --- selector type must be in the IO monad"
  
            -- isUnit is a boolean which is true if pureType is unit.  This 
            -- will eventually be used by makeMarsheller and marshellerName
            isUnit = pureType == ConT ''()

            (resultRetained, doctoredTypeSig) = doctorType typeSig className
            
            -- resultType --
            -- given a type, returns the return value, all applications
            resultType (ForallT _ _ ty) = resultType ty
            resultType ((ArrowT `AppT` _) `AppT` rest) = resultType rest
            resultType other = other
            
            -- countArgs
            -- return the number of arguments that the function takes.
            countArgs (ForallT _ _ ty) = countArgs ty
            countArgs ((ArrowT `AppT` _) `AppT` rest) = 1 + countArgs rest
            countArgs _ = 0
            
            -- substitute the result of the second argument for the first from 
            -- within arrows and foralls only.
            replaceResult new (ForallT vars ctxt ty) = ForallT vars ctxt (replaceResult new ty)
            replaceResult new ((ArrowT `AppT` arg) `AppT` rest) =
                (ArrowT `AppT` arg) `AppT` replaceResult new rest
            replaceResult new _ = new

            -- this takes:
            -- forall <names> (context). (forall <names>' (context'). type')
            -- and turns it into
            -- forall <names> ++ <names>' (context ++ context'). type'
            -- Thus it "flattens" foralls
            liftForalls (ForallT names cxt ty)
                = case liftForalls ty of
                    ForallT names' cxt' ty'
                        -> ForallT (nub (names ++ names')) (nub (cxt ++ cxt')) ty'
                    ty' -> ForallT names cxt ty'
            liftForalls other = other
            
            -- this takes a type and a class name.
            --
            -- It starts by producing one of two polymorphic type declarations:
            -- if needInstance is true:
            -- forall target instance =>
            --     (className target, ClassAndObject target instance) .
            --     (target-> covariantResult)
            -- if needInstances is false:
            --  forall target =>  (className target) .
            --      (target-> covariantResult)
            -- covarianResult is determined by:
            --
            doctorType ty className = 
                    (
                        retained,
                        liftForalls $
                        applyCxts (classCxt : extraCxts) $
                        replaceResult (
                            (ArrowT `AppT` (fromMaybe (VarT $ mkName "target") targetType))
                            `AppT` covariantResult
                        ) ty
                    )
                where
                    (retained, extraCxts, targetType, covariantResult) =
                        doctorCovariant $ resultType ty
                    applyCxts [] = id
                    applyCxts ((ts, p):rest) = ForallT (map (PlainTV . mkName) ts) [p] . applyCxts rest
                    
                    classCxt = (["target"], ClassP (mkName className) [VarT (mkName "target")])
            -- 
            -- doctorCovariant --
            -- The values returned in the 4-tuple are:
            -- retained: true if this object has been retained after this 
            -- method call
            -- needInstance: true if this method needs an instance passed in 
            -- (eg. is it "static")
            -- targetType:
            -- covarientResult: This is the covarient return type.  It's 
            -- "target" 
            --
            -- the first form handles almost all the types that deal with 
            -- reference count types.  Retained is handled below
            --
            -- A Covarient type is a type that returns an object.
            -- typical Objective C object returns are Covariant because you can 
            -- return any type more specific than the declared one, and assign 
            -- upwards as well.
            -- A CovariantInstance type that is a factory method.  This is a 
            -- Covariant return that also initializes a new object.
            --
            -- However, currently Covariant and Covariant instance are not 
            -- detected.  It has to be specified in the binding script.
            doctorCovariant (ConT con)
                | con == ''Covariant =
                    (False, [covariantCxt], Nothing, covariantTarget)
                | con == ''CovariantInstance =
                    (False, [instCxt], Nothing, VarT $ mkName "inst")
                | con == ''Allocated =
                    (False, [instCxt], Nothing, newlyAllocated "inst")
                | con == ''Inited =
                    (True, [covariantCxt], Just (newlyAllocated "target"), covariantTarget)
                where
                    newlyAllocated ty = ConT ''NewlyAllocated `AppT` VarT (mkName ty)
                    covariantTarget = ConT ''CovariantReturn  `AppT` VarT (mkName "target")
                    instCxt  = (["target", "inst"],
                        ClassP ''ClassAndObject [VarT (mkName "target"),
                                                 VarT (mkName "inst")])
                    covariantCxt = (["target"],
                        ClassP ''CovariantReturnTarget [VarT (mkName "target")])
            -- 
            doctorCovariant (ConT con `AppT` ty) | con == ''Retained =
                    (True,inst', target', ty')
                where (_,inst', target', ty') = doctorCovariant ty

            doctorCovariant (t1 `AppT` t2) =
                    (retained1 || retained2, needInst1 ++ needInst2, target1 `mplus` target2, t1' `AppT` t2')
                where (retained1, needInst1, target1, t1') = doctorCovariant t1
                      (retained2, needInst2, target2, t2') = doctorCovariant t2

            doctorCovariant other = (False, [], Nothing, other)

            -- Reduce the type to a form that can be used for creating a libffi CIF
            -- using the ObjCIMPType type class:

            simplifyType (ForallT _ _ ty) = simplifyType ty
            simplifyType ((ArrowT `AppT` arg) `AppT` rest) = (ArrowT `AppT` replaceVarByUnit arg)
                                                            `AppT` simplifyType rest
            simplifyType (ConT con `AppT` x) | con == ''IO = ConT ''IO `AppT` replaceVarByUnit x
            simplifyType x = replaceVarByUnit x

            replaceVarByUnit (VarT _) = ConT ''ID `AppT` ConT ''()
            replaceVarByUnit (ConT con `AppT` ty)
                | con == ''NewlyAllocated  = replaceVarByUnit ty
                | con == ''CovariantReturn = replaceVarByUnit ty
            replaceVarByUnit (ConT cls `AppT` VarT _) = ConT cls `AppT` ConT ''()
            replaceVarByUnit x = x


            makeImpType ty = replaceResult (
                                (ArrowT `AppT` fromMaybe (VarT $ mkName "target") target')
                                `AppT` covariantResult
                            ) ty'
                where
                    ty' = simplifyType ty
                    (_retained, _needInstance, target', covariantResult) =
                        doctorCovariant $ resultType ty'
            
            selInfoMaker | resultRetained = [| mkSelectorInfoRetained |]
                         | otherwise      = [| mkSelectorInfo |]
            
        sequence $ [
                
                -- $(selectorName) = getSelectorForName "name"
                -- valD (VarP selectorName) (normalB [| selectorInfoSel $(infoVar) |]) [],
                
                
                -- $(infoName) :: SelectorInfo (ForeignSig (DropSelTarget $(...)))
                sigD (mkName $ infoName)
                    [t| SelectorInfo (ForeignSig (DropSelTarget 
                        $(return $ simplifyType doctoredTypeSig))) |],
                
                -- $(infoName) = ...
                valD (varP $ mkName $ infoName) (normalB
                        [|
                            let n = $(stringE name)
                            in $(selInfoMaker) n
                                                $(if haskellName == name
                                                        then [|n|]
                                                        else stringE haskellName)
                        |]) [],
                    
                -- type $(imptypeName) target inst = arg1 -> arg2 -> target -> IO result
                tySynD (mkName imptypeName) (map (PlainTV . mkName) ["target","inst"])
                    (return $ makeImpType typeSig),
                
                -- class Object a => $(className) a
                classD (cxt [classP ''MessageTarget [varT (mkName "a")]])
                    (mkName className) [PlainTV $ mkName "a"] [] [],
                
                -- instance $(className) a => $(className) (SuperTarget a)
                instanceD (cxt [classP (mkName className) [varT (mkName "a")]
                               ,classP ''Object           [varT (mkName "a")]])
                    (conT (mkName className) `appT` (conT ''SuperTarget `appT` varT (mkName "a")))
                    [],
                
                sigD (mkName haskellName) $ return doctoredTypeSig,
                
                if nArgs > marshallersUpTo || resultRetained
                    then makeMarshaller (Just $ mkName infoName) (mkName haskellName) nArgs
                                        isUnit isPure resultRetained
                    else valD (varP $ mkName haskellName) (normalB [|
                            $(varE $ 
                                    marshallerName nArgs isUnit `fromSameModuleAs_v`
                                        'marshallersUpTo
                                )
                                $(varE $ mkName infoName)
                        |]) []
            ]

declareSelector name typeSig = declareRenamedSelector name (mangleSelectorName name) typeSig

