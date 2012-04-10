module HOC.TH(
        module Language.Haskell.TH,
        mkNameG_v,
        mkNameG_tc,
        mkNameG_d,
        whereQ,
        fromSameModuleAs_tc,
        fromSameModuleAs_v
    ) where

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
