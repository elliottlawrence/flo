{-# LANGUAGE RecordWildCards, MultiParamTypeClasses #-}
module HaskellProgram where

import Convertible
import FloGraph
import FloProgram

import Data.List (intercalate)

data HaskellExpr = HaskellLit Literal
                 | HaskellFun Name
                 | HaskellCons Name
                 | HaskellAp HaskellExpr HaskellExpr
                 | HaskellLambda [Input] HaskellExpr
                 | HaskellLet [HaskellDef] HaskellExpr

instance Show HaskellExpr where
  show (HaskellLit lit) = show lit
  show (HaskellFun fun) = fixName fun
  show (HaskellAp e1 e2) = show e1 ++ " " ++ e2'
    where e2' | isAtomic e2 = show e2
              | otherwise = "(" ++ show e2 ++ ")"
  show (HaskellLambda inputs expr) = '\\' : showInputs inputs ++ " -> " ++
    show expr
  show (HaskellLet ld le)
    = "let {" ++ intercalate "; " (map show ld) ++ "} in " ++ show le

isAtomic :: HaskellExpr -> Bool
isAtomic (HaskellLit _) = True
isAtomic (HaskellFun _) = True
isAtomic _ = False

showInputs :: [Input] -> String
showInputs inputs = unwords $ map (fixName . iName) inputs

data HaskellDef = HaskellDef {
  hdName :: Name,
  hdInputs :: [Input],
  hdExpr :: HaskellExpr
}

instance Show HaskellDef where
  show HaskellDef{..}
    = fixName hdName ++ space ++ showInputs hdInputs ++ " = " ++ show hdExpr
    where space | null hdInputs = ""
                | otherwise = " "

data HaskellModule = HaskellModule {
  hmName :: Name,
  hmDefs :: [HaskellDef],
  hmImports :: [Name]
}

instance Show HaskellModule where
  show HaskellModule{..} = "module " ++ hmName ++ " where\n\n" ++ imports' ++
    intercalate "\n\n" (map show hmDefs)
    where imports' = intercalate "\n" (map ("import " ++) hmImports) ++ "\n\n"

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
