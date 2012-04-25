module HOC.TH where

import Language.Haskell.TH
import Language.Haskell.TH.Extras
import Language.Haskell.TH.Syntax

whereQ :: ([Q Dec] -> Q a) -> Q [Dec] -> Q a
header `whereQ` declsQ = do
    decls <- declsQ
    header (map return (genericalizeDecs decls))

fromSameModuleAs_tc :: String -> Name -> Name
fromSameModuleAs_tc = fromSameModule TcClsName
fromSameModuleAs_v :: String -> Name -> Name
fromSameModuleAs_v = fromSameModule VarName

fromSameModule :: NameSpace -> String -> Name -> Name
fromSameModule ns s n
  = Name (mkOccName s) $
    case n of
        Name _ (NameG _ pkg mod) -> NameG ns pkg mod
        Name _ other -> other

-- return the number of arguments that the function takes.
countArgs :: Type -> Int
countArgs (ForallT _ _ ty) = countArgs ty
countArgs ((ArrowT `AppT` _) `AppT` rest) = 1 + countArgs rest
countArgs _ = 0

-- resultType --
-- given a type, returns the return value, all applications
resultType :: Type -> Type
resultType (ForallT _ _ ty) = resultType ty
resultType ((ArrowT `AppT` _) `AppT` rest) = resultType rest
resultType other = other

-- substitute the result of the second argument for the first from 
-- within arrows and foralls only.
replaceResult :: Type -> Type -> Type
replaceResult new (ForallT vars ctxt ty) = ForallT vars ctxt (replaceResult new ty)
replaceResult new ((ArrowT `AppT` arg) `AppT` rest) =
    (ArrowT `AppT` arg) `AppT` replaceResult new rest
replaceResult new _ = new
