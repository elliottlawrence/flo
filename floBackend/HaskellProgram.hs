{-# LANGUAGE RecordWildCards, MultiParamTypeClasses, TypeSynonymInstances,
  FlexibleInstances #-}
module HaskellProgram where

import Convertible
import FloGraph
import FloProgram hiding (isAtomic)
import Pretty

import Data.List (intercalate)
import Text.PrettyPrint

data HaskellExpr = HaskellLit Literal
                 | HaskellFun Name
                 | HaskellCons Name
                 | HaskellAp HaskellExpr HaskellExpr
                 | HaskellLambda [Input] HaskellExpr
                 | HaskellLet [HaskellDef] HaskellExpr

data HaskellDef = HaskellDef {
  hdName :: Name,
  hdInputs :: [Input],
  hdExpr :: HaskellExpr
}

data HaskellModule = HaskellModule {
  hmName :: Name,
  hmDefs :: [HaskellDef],
  hmImports :: [Name]
}

type HaskellProgram = [HaskellModule]

instance Convertible FloModule HaskellModule where
  convert FloModule{..} = HaskellModule fmName (convert fmDefs) imports
    where imports | fmName /= "Prologue" = ["Prologue"]
                  | otherwise = []

instance Convertible FloDef HaskellDef where
  convert FloDef{..} = HaskellDef fdName fdInputs (convert fdExpr)

instance Convertible FloExpr HaskellExpr where
  convert (FloLit lit) = HaskellLit lit
  convert (FloFun name _) = HaskellFun $ fixName name
  convert (FloCons name _) = HaskellCons name
  convert (FloAp e1 e2) = HaskellAp (convert e1) (convert e2)
  convert (FloLambda inputs expr) = HaskellLambda inputs (convert expr)
  convert (FloLet ld le) = HaskellLet (convert ld) (convert le)

{- To avoid name clashes with existing things in Haskell -}
fixName :: String -> String
fixName "if" = "iff"
fixName "then" = "thenn"
fixName "else" = "elsee"
fixName name = name

-- Pretty printing
instance Pretty HaskellExpr where
  pp (HaskellLit lit) = pp lit
  pp (HaskellFun fun) = text $ fixName fun
  pp (HaskellAp e1 e2) = pp e1 <+> e2'
    where e2' | isAtomic e2 = pp e2
              | otherwise = parens $ pp e2
  pp (HaskellLambda inputs expr)
    = char '\\' <> pp inputs <+> text "->" <+> pp expr
  pp (HaskellLet ld le) = text "let" <+>
    braces (vcat $ punctuate semi (map pp ld)) <+> text "in" $$ pp le

isAtomic :: HaskellExpr -> Bool
isAtomic (HaskellLit _) = True
isAtomic (HaskellFun _) = True
isAtomic _ = False

instance Pretty HaskellDef where
  pp HaskellDef{..}
    = hsep [text (fixName hdName), pp hdInputs, equals, pp hdExpr]

instance Pretty HaskellModule where
  pp HaskellModule{..} = text "module" <+> text hmName <+>
    text "where" $+$ imports' $+$ vcat (map pp hmDefs)
    where imports' = vcat $ map ((text "import" <+>) . text) hmImports

instance Pretty HaskellProgram where
  pp modules = vcat $ map pp modules
