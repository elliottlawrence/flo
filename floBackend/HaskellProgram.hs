{-# LANGUAGE RecordWildCards, MultiParamTypeClasses #-}
module HaskellProgram where

import Convertible
import FloGraph
import FloProgram

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

data HaskellProgram = HaskellProgram [HaskellModule] deriving Show

instance Convertible FloProgram HaskellProgram where
  convert (FloProgram modules) = HaskellProgram $ convert modules

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
instance Show HaskellExpr where
  show = render . ppHaskellExpr

ppHaskellExpr :: HaskellExpr -> Doc
ppHaskellExpr (HaskellLit lit) = ppLiteral lit
ppHaskellExpr (HaskellFun fun) = text $ fixName fun
ppHaskellExpr (HaskellAp e1 e2) = ppHaskellExpr e1 <+> e2'
  where e2' | isAtomic e2 = ppHaskellExpr e2
            | otherwise = parens $ ppHaskellExpr e2
ppHaskellExpr (HaskellLambda inputs expr) = char '\\' <> ppInputs inputs <+>
  text "->" <+> ppHaskellExpr expr
ppHaskellExpr (HaskellLet ld le) = text "let" <+>
  braces (vcat $ punctuate semi (map ppHaskellDef ld)) <+>
  text "in" $$ ppHaskellExpr le

isAtomic :: HaskellExpr -> Bool
isAtomic (HaskellLit _) = True
isAtomic (HaskellFun _) = True
isAtomic _ = False

ppInputs :: [Input] -> Doc
ppInputs inputs = hsep $ map (text . fixName . iName) inputs

instance Show HaskellDef where
  show = render . ppHaskellDef

ppHaskellDef :: HaskellDef -> Doc
ppHaskellDef HaskellDef{..} = hsep [text (fixName hdName), ppInputs hdInputs,
  equals, ppHaskellExpr hdExpr]

instance Show HaskellModule where
  show = render . ppHaskellModule

ppHaskellModule :: HaskellModule -> Doc
ppHaskellModule HaskellModule{..} = text "module" <+> text hmName <+>
  text "where" $+$ imports' $+$ vcat (map ppHaskellDef hmDefs)
  where imports' = vcat $ map ((text "import" <+>) . text) hmImports
