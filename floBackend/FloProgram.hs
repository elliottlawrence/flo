{-# LANGUAGE RecordWildCards, MultiParamTypeClasses, FlexibleInstances,
    TupleSections #-}
module FloProgram where

import Convertible
import FloGraph
import Pretty

import Control.Arrow (second)
import qualified Data.IntMap as IntMap
import Data.List ((\\), find)
import Data.Maybe (fromMaybe)
import Text.PrettyPrint
import Text.Regex.Posix

data Literal = LitInt Int | LitFloat Double | LitChar Char | LitString String

data FloExpr = FloLit Literal                     -- Literals
             | FloFun Name [Input]                -- Functions
             | FloCons Name [Input]               -- Constructors
             | FloAp FloExpr FloExpr              -- Applications
             | FloLambda [Input] FloExpr          -- Lambdas
             | FloLet [FloDef] FloExpr            -- Let expressions

data FloDef = FloDef {
  fdName :: Name,
  fdInputs :: [Input],
  fdExpr :: FloExpr
}

data FloModule = FloModule {
  fmName :: Name,
  fmDefs :: [FloDef]
}

type FloProgram = [FloModule]

{- The following functions convert the graphical representation of a program
   into a format composed of expressions and definitions. -}
instance Convertible FloGraph FloProgram where
  convert (FloGraph modules) = convert modules

instance Convertible Module FloModule where
  convert Module{..} = FloModule mName $ convert mDefs

{- The expression component of a box definition is the expression determined by
   the output box. In addition, if a box has local definitions, these are
   captured by wrapping the box definition in a let expression. -}
instance Convertible BoxDef FloDef where
  convert bd@BoxDef{..} = FloDef bName bInputs $
    if null localDefs then boxExpr else FloLet (convert localDefs) boxExpr
    where BoxInterface{..} = boxInterface
          boxExpr = convert (bd, getOutput bd)

{- Converts the box with the given output, defined in the given box definition, to
   a Flo Expression. -}
instance Convertible (BoxDef, Output) FloExpr where
  convert (bd@BoxDef{..}, Output{..})
    | oParentID == -1 = FloFun oEndInputName []
    | otherwise = fromMaybe (createAp FloFun) (convert bi)
    where bi@BoxInterface{..} = fromMaybe (error "Box not found") $
                                IntMap.lookup oParentID boxes
          {- To create an expression out of a function or constructor, convert
             all of its inputs to expressions and then apply them to the
             function. For constant applicative forms, no inputs need be
             applied. -}
          createAp constructor | null unappliedInputs = rhs
                               | otherwise = FloLambda unappliedInputs rhs
            where unappliedInputs = getUnappliedInputs bd oParentID
                  appliedInputs = getAppliedInputs bd oParentID
                  appliedExprs = map (second $ convert . (bd,)) appliedInputs
                  apps = applyExprs bInputs appliedExprs
                  rhs = foldl1 FloAp $ constructor bName bInputs : apps

{- Applies the expressions to the list of inputs -}
applyExprs :: [Input] -> [(Input, FloExpr)] -> [FloExpr]
applyExprs (i@Input{..}:inputs) apps =
  case find ((== i) . fst) apps of
    Just ie -> snd ie : applyExprs inputs apps
    Nothing -> FloFun iName [] : applyExprs inputs apps
applyExprs [] apps = []

{- A literal is simply the box's name. -}
instance Convertible BoxInterface (Maybe FloExpr) where
  convert BoxInterface{..}
    | isLitString bName = justLit $ LitString (init $ tail bName)
    | isLitChar bName = justLit $ LitChar (read bName)
    | isLitInt bName = justLit $ LitInt (read bName)
    | isLitFloat bName = justLit $ LitFloat (read bName)
    | otherwise = Nothing
    where justLit = Just . FloLit

isLitString :: String -> Bool
isLitString = (=~ "^\".*\"$")   -- ^".*"$

isLitChar :: String -> Bool
isLitChar = (=~ "^'[^\\']'$|^'\\\\['trn]'$|^'\\\\\\\\'$")
              -- ^'[^\']'$|^'\\['trn]'$|^'\\\\'$

isLitInt :: String -> Bool
isLitInt = (=~ "^[0-9]+$")    -- ^[0-9]+$

isLitFloat :: String -> Bool
isLitFloat = (=~ "^[0-9]*\\.[0-9]+$")     -- ^[0-9]*\.[0-9]+$

-- Pretty printing
instance Pretty Literal where
  pp (LitInt i) = int i
  pp (LitFloat d) = double d
  pp (LitChar c) = char c
  pp (LitString s) = char '"' <> text s <> char '"'

instance Pretty FloDef where
  pp FloDef{..} = text fdName <+> hsep (map (text . iName) fdInputs) <+>
    equals <+> nest 4 (pp fdExpr)

instance Pretty FloExpr where
  pp (FloLit lit) = pp lit
  pp (FloFun name _) = text name
  pp (FloCons name _) = text name
  pp (FloAp e1 e2) = pp e1 <+> e2'
    where e2' | isAtomic e2 = pp e2
              | otherwise = parens $ pp e2
  pp (FloLambda inputs expr) = char '\\' <> pp inputs <+> text "->" <+> pp expr
  pp (FloLet ld le) = text "let" <+> braces (vcat $ punctuate semi (map pp ld))
    <+> text "in" $$ pp le

isAtomic :: FloExpr -> Bool
isAtomic (FloLit _) = True
isAtomic (FloFun _ _) = True
isAtomic _ = False

instance Pretty [Input] where
  pp inputs = hsep $ map (text . iName) inputs

instance Pretty FloModule where
  pp FloModule{..} = text "FloModule" <+> text fmName <+> lbrace $$
    nest 4 (vcat (map pp fmDefs) $$ rbrace)

instance Pretty FloProgram where
  pp = vcat . map pp
