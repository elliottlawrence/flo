{-# LANGUAGE RecordWildCards, MultiParamTypeClasses, TypeSynonymInstances,
  FlexibleInstances #-}
module HaskellProgram where

import Convertible
import FloGraph
import FloProgram hiding (isAtomic)
import Pretty

import Data.List (intercalate)
import qualified Data.Map as Map
import Text.PrettyPrint.Leijen hiding (Pretty)

data HaskellExpr = HaskellLit Literal
                 | HaskellVar Name
                 | HaskellCons Name
                 | HaskellAp HaskellExpr HaskellExpr
                 | HaskellLambda [Name] HaskellExpr
                 | HaskellLet [HaskellDef] HaskellExpr
                 | HaskellCase HaskellExpr [(HaskellExpr, HaskellExpr)]

data HaskellDef = HaskellDef {
  hdName :: Name,
  hdInputs :: [Name],
  hdExpr :: HaskellExpr
}

data HaskellDataType = HaskellDataType {
  dtName :: Name,
  dtTyVars :: [Name],
  dtDataConses :: [HaskellDataCons]
}

data HaskellDataCons = HaskellDataCons {
  hdcName :: Name,
  hdcFields :: [Type]
}

data HaskellModule = HaskellModule {
  hmName :: Name,
  hmDataTypes :: [HaskellDataType],
  hmDefs :: [HaskellDef],
  hmImports :: [Name]
}

type HaskellProgram = [HaskellModule]

instance Convertible FloModule HaskellModule where
  convert FloModule{..}
    = HaskellModule fmName (collectDataTypes $ convert fmDataConses)
    (convert fmDefs) imports
    where imports = "qualified Prelude as Hask" : others
          others | fmName /= "Prologue" = ["Prologue"]
                 | otherwise = []

{- Combines data constructors with the same type into one declaration -}
collectDataTypes :: [HaskellDataType] -> [HaskellDataType]
collectDataTypes dataTypes = dataTypes'
  where dataTypesMap = Map.fromListWith (++) [((dtName, dtTyVars), dtDataConses)
          | HaskellDataType{..} <- dataTypes]
        dataTypes' = Map.elems $ Map.mapWithKey (\(name, tyVars) dataConses ->
          HaskellDataType name tyVars dataConses) dataTypesMap

instance Convertible FloDef HaskellDef where
  convert FloDef{..} = HaskellDef (fixName fdName) (map fixName fdInputs)
    (convert fdExpr)

instance Convertible FloExpr HaskellExpr where
  convert (FloLit lit) = HaskellLit lit
  convert (FloVar name) = HaskellVar $ fixName name
  convert (FloCons name) = HaskellCons name
  convert (FloAp e1 e2) = HaskellAp (convert e1) (convert e2)
  convert (FloLambda inputs expr) = HaskellLambda inputs (convert expr)
  convert (FloLet ld le) = HaskellLet (convert ld) (convert le)
  convert (FloCase e alts) = HaskellCase (convert e) (convert alts)

{- Initially, we just convert each data constructor into a new data type. Later
   on we collect the data constructors with the same types together. -}
instance Convertible FloDataCons HaskellDataType where
  convert FloDataCons{..} = HaskellDataType tyName tyVars'
    [HaskellDataCons dcName dcFields]
    where TypeCons tyName tyVars = dcType
          tyVars' = map (\(TypeCons name []) -> name) tyVars

{- To avoid name clashes with existing things in Haskell -}
fixName :: String -> String
fixName "if" = "iff"
fixName "then" = "thenn"
fixName "else" = "elsee"
fixName name = name

-- Pretty printing
instance Pretty HaskellExpr where
  pp (HaskellLit lit) = pp lit
  pp (HaskellVar fun) = text fun
  pp (HaskellCons cons) = text cons
  pp (HaskellAp e1 e2) = pp e1 <+> maybeParens (not $ isAtomic e2) (pp e2)
  pp (HaskellLambda inputs expr)
    = char '\\' <> pp inputs <+> text "->" <+> pp expr
  pp (HaskellLet ld le) = text "let" <+> braces
    (vcat $ punctuate semi (map pp ld)) </> text "in" <+> align (pp le)
  pp (HaskellCase e alts) = text "case" <+> pp e <+> text "of" </>
    align (pp alts)

isAtomic :: HaskellExpr -> Bool
isAtomic (HaskellLit _) = True
isAtomic (HaskellVar _) = True
isAtomic (HaskellCons _) = True
isAtomic _ = False

instance Pretty (HaskellExpr, HaskellExpr) where
  pp (e1, e2) = pp e1 <+> text "->" <+> align (pp e2)

instance Pretty HaskellDef where
  pp HaskellDef{..} = hsep [text hdName, hsep $ map pp hdInputs, equals,
    align $ pp hdExpr]
  ppList = vcat . punctuate line . map pp

instance Pretty HaskellDataType where
  pp HaskellDataType{..} = hsep [text "data", text dtName, pp dtTyVars, equals,
    pp dtDataConses]

instance Pretty HaskellDataCons where
  pp HaskellDataCons{..} = text hdcName <> fields'
    where fields' | null hdcFields = empty
                  | otherwise = space <> pp hdcFields
  ppList = hsep . punctuate (text " |") . map pp

instance Pretty HaskellModule where
  pp HaskellModule{..} = vcat [text "module" <+> text hmName
    <+> text "where", imports', dataTypes', pp hmDefs]
    where imports' = vcat $ map ((text "import" <+>) . text) hmImports
          dataTypes' | null hmDataTypes = empty
                     | otherwise = line <> pp hmDataTypes <> line
