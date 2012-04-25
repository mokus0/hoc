{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FunctionalDependencies #-}
module HOC.ExportClass
    ( ClassMember(..)
    , IVar(..)
    , getInstanceMVar
    , getIVar, setIVar
    , getAndSetIVar
    , exportClass
    ) where

import Control.Concurrent.MVar
import Data.Char                    ( toUpper )
import Data.Dynamic                 ( toDyn, fromDynamic )
import Data.Maybe                   ( mapMaybe )
import Data.Typeable                ( Typeable )
import Foreign.LibFFI.Experimental  ( cif, pokeRet )
import Foreign.ObjC
import Foreign.ObjC.HSObject        ( registerHSObjectClass )
import Foreign.Ptr                  ( Ptr, castPtr )
import HOC.Arguments                ( objcOutRet )
import HOC.CBits
import HOC.ID                       ( ID(..), nil, idData )
import HOC.Invocation               ( getMarshalledArgument )
import HOC.MessageTarget            ( Object(..) )
import HOC.SelectorMarshaller       ( SelectorInfo(..) )
import HOC.TH

data ClassMember =
        InstanceMethod Name
    |   ClassMethod Name
    |   Outlet String TypeQ
    |   InstanceVariable String TypeQ ExpQ

class (Object cls, Typeable ivars) => InstanceVariables cls ivars
      | cls -> ivars, ivars -> cls
      where
    initializeInstanceVariables :: IO ivars

newtype IVar i v = IVar (i -> MVar v)

getInstanceVariablesForObject :: InstanceVariables cls iv => cls -> iv
getInstanceVariablesForObject = head . mapMaybe fromDynamic . idData . toID

getInstanceMVar :: InstanceVariables cls iv => IVar iv a -> cls -> MVar a
getInstanceMVar (IVar extract) obj = extract $ getInstanceVariablesForObject obj

getIVar :: InstanceVariables cls iv => IVar iv a -> cls -> IO a
getIVar ivar obj = readMVar (getInstanceMVar ivar obj)
setIVar :: InstanceVariables cls iv => IVar iv a -> a -> cls -> IO ()
setIVar ivar val obj = modifyMVar_ (getInstanceMVar ivar obj) (\_ -> return val)

getAndSetIVar :: InstanceVariables cls iv => IVar iv a -> a -> cls -> IO a
getAndSetIVar ivar val obj = swapMVar (getInstanceMVar ivar obj) val

getAsID :: InstanceVariables cls iv => IVar iv (ID a) -> cls -> IO (ID ())
getAsID ivar obj = getIVar ivar obj >>= return . toID

setAsID :: InstanceVariables cls iv => IVar iv (ID a) -> ID () -> cls -> IO ()
setAsID ivar val obj = setIVar ivar (fromID val) obj

type SetVarImpType target inst = ID () -> target -> IO ()
type GetVarImpType target inst = target -> IO (ID ())

exportClass :: String -- ^ Name of class you're exporting, e.g. "MyDocument"
            -> String -- ^ A prefix for function names which are methods 
                      --   belonging to this class, e.g. "md_"
            -> [ClassMember] -- ^ A list of class members, such as outlets
                             --   and instance variables
            -> Q [Dec] -- ^ A Haskell declaration, which can be spliced in
                       --   with Template Haskell's $(...) syntax
exportClass name prefix members = sequence $ [
        sigD (mkName exportFunName) [t| IO () |],
        valD (varP $ mkName exportFunName)
            (normalB (mkClassExportAction name prefix members)) [],
        dataD (cxt []) (mkName instanceDataName) []
            [normalC (mkName instanceDataName) strictTypes] [''Typeable],
        instanceD (cxt []) (conT ''InstanceVariables
                            `appT` clsTy `appT` instTy) 
                [
--                  All we want to do is this:
--                      initializeInstanceVariables = $initIVars
--                  But we want the name initializeInstanceVariables refer directly
--                  to this module, so that we don't have to export it, but keep it
--                  private.
                    do e <- initIVars
                       return (ValD (VarP 'initializeInstanceVariables) (NormalB e) [])
                ]
    ] ++ declaredIVars
    where
        exportFunName = "initializeClass_" ++ name
        instanceDataName = name ++ "_IVARS"
        strictTypes = map (strictType (return IsStrict)) wrappedIvarTypes
        ivars = [ (name, ty, [| nil |]) | Outlet name ty <- members ]
             ++ [ (name, ty, initial)   | InstanceVariable name ty initial <- members ]
        wrappedIvarTypes = [ conT ''MVar `appT` ty | (_,ty,_) <- ivars ]
        ivarNames = [ name | (name,_,_) <- ivars ]
        clsTy = conT (mkName name) `appT` [t| () |]
        instTy = conT (mkName instanceDataName)
        nIVars = length ivarNames
        
        declaredIVars = concat $ zipWith declareIVar ivars [1..]
        declareIVar (name, ty, _) n = [
            sigD (mkName ('_':name))
              (conT ''IVar `appT` (conT $ mkName instanceDataName) `appT` ty)
          , valD (varP $ mkName ('_':name))
              (normalB [| IVar $(getNth n) |]) []
          ]
            where
                getNth n = lamE [conP (mkName instanceDataName) (args n)]
                                (varE $ mkName $ "arg" ++ show n)
                args n = [ mkVarP (n == i) $ "arg" ++ show i | i <- [1..nIVars] ]
        
        initIVars = doE (map initIVar ivars ++ [noBindS [| return $wrap |]])
            where
                wrap = foldl appE (conE $ mkName instanceDataName) (map (varE.mkName) ivarNames)
                initIVar (ivar,ty,initial) = bindS (varP $ mkName ivar) [| newMVar $initial |]

-- | Declare a variable (with a preceeding _ unless it is used)
mkVarP :: Bool -> String -> PatQ
mkVarP used = varP . mkName . (if used then id else ('_':))

data Method = ImplementedMethod Name
            | GetterMethod String
            | SetterMethod String

mkClassExportAction name prefix members =
        [| 
            do
                super <- objc_getClass $(varE $ mkName $ "super_" ++ name)
                cls   <- objc_allocateClassPair super name 0
                meta  <- object_getClass (castPtr cls)
                
                $(addMethods [| cls  |] instanceType instanceMethods) 
                $(addMethods [| meta |] classType    classMethods)
                
                registerHSObjectClass cls (Just ($typedInitIvars >>= return . toDyn))
        |]
    where
        instanceType = conT $ mkName name
        classType    = conT $ mkName $ name ++ "Class"
        
        typedInitIvars = [|initializeInstanceVariables|]
            `sigE` (conT ''IO `appT` conT (mkName $ name ++ "_IVARS"))
    
        classMethods = [ ImplementedMethod n | ClassMethod n <- members ] 

        explicitInstanceMethods = [ n | InstanceMethod n <- members ]         
        instanceMethodNames = map nameBase explicitInstanceMethods
        instanceMethods = 
                map ImplementedMethod explicitInstanceMethods
             ++ concat [ [ GetterMethod ivar, SetterMethod ivar ]
                       | Outlet ivar _ <- members
                       , ivar `notElem` instanceMethodNames ]
        
            -- GHC fails with Prelude.last if we pass it an empty doE
        addMethods _clsE _targetType [] = [| return () |]  
        addMethods  clsE  targetType methods =
            doE (map (noBindS . addMethod clsE targetType) methods)
                
        addMethod clsE targetType (ImplementedMethod selName)
            = do
                VarI _ t _ _ <- reify $ selName
                let arrowsToList (AppT (AppT ArrowT a) b)
                        = a : arrowsToList b
                    arrowsToList (AppT (ConT c) b)
                        | c == ''IO
                        = [b]
                    arrowsToList (ForallT _ _ a)
                        = arrowsToList a
                    ts = arrowsToList t
                
                    nArgs = length ts - 2  -- subtract target and result
                    isUnit = last ts == ConT ''()
                
                addMethod' clsE targetType methodBody
                              nArgs isUnit impTypeName selExpr
                              retainedExpr
            where
                methodBody = varE $ mkName $ prefix ++ nameBase selName
                
                impTypeName = ("ImpType_" ++ nameBase selName)
                                `fromSameModuleAs_tc` selName
                infoExpr = varE (("info_" ++ nameBase selName)
                                `fromSameModuleAs_v` selName)
                
                selExpr      = [| selectorInfoSel            $infoExpr |]
                retainedExpr = [| selectorInfoResultRetained $infoExpr |]
        
        addMethod clsE targetType (GetterMethod ivarName) =
                addMethod' clsE targetType
                              ([| getAsID |] `appE` varE (mkName ('_':ivarName)))
                              0 False ''GetVarImpType sel [| False |]
            where sel = [| getSEL ivarName :: SEL (IO (Ptr ObjCObject)) |]
            
        addMethod clsE targetType (SetterMethod ivarName) =
                addMethod' clsE targetType
                              ([| setAsID |] `appE` varE (mkName ('_':ivarName)))
                              1 True ''SetVarImpType sel [| False |]
            where
                sel = [| getSEL setterName :: SEL (Ptr ObjCObject -> IO ()) |]
                setterName = "set" ++ toUpper (head ivarName) : tail ivarName ++ ":"
        
        addMethod' clsE targetType methodBody
                       nArgs isUnit impTypeName selE retainedExpr =
            [| do
                    hsImp <- wrapHsIMP $(lamE (map (uncurry mkVarP) [(False,"cif"),(not isUnit,"ret"),(True,"args")]) marshal)
                    class_addMethod $clsE $selE =<< newIMP cif hsImp
            |]
            where
                argsE = varE $ mkName "args"
                retE  = varE $ mkName "ret"
                
                marshal = [| do recordHOCEvent kHOCEnteredHaskell $argsE
                                exc <- wrapException $marshal'
                                recordHOCEvent kHOCAboutToLeaveHaskell $argsE
                                return exc
                          |]
                marshal' = doE $ 
                    getArg ("slf",0)
                        : map getArg (zip arguments [2..])
                        ++  [ noBindS [| recordHOCEvent kHOCImportedArguments $argsE |]
                            , noBindS invokeAndReturn
                            ]
                
                arguments = [ "arg" ++ show i | i <- [1..nArgs] ]
                
                invokeAndReturn
                    | isUnit    = typedBodyWithArgs
                    | otherwise =
                        [| do result <- $typedBodyWithArgs
                              recordHOCEvent kHOCAboutToExportResult $argsE
                              pokeRet (objcOutRet $retainedExpr) (castPtr $retE) result
                        |]
                
                typedBodyWithArgs = appsE (typed methodBody
                    : map (varE.mkName)(arguments ++ ["slf"])) 
                
                typed thing = sigE thing
                    (conT impTypeName
                     `appT` (targetType   `appT` [t| () |])
                     `appT` (instanceType `appT` [t| () |]))
                
                getArg (argname, argnum) =
                    bindS (varP (mkName argname))
                          [| getMarshalledArgument $argsE argnum |]
