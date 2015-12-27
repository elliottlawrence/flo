{-# LANGUAGE RecordWildCards, MultiParamTypeClasses, FlexibleInstances,
    TupleSections #-}
module FloProgram where

import Convertible
import FloGraph

import Control.Arrow (second)
import qualified Data.IntMap as IntMap
import Data.List ((\\), find, intercalate)
import Data.Maybe (fromMaybe)
import Text.Regex.Posix

data Literal = LitInt Int | LitFloat Double | LitChar Char | LitString String

instance Show Literal where
  show (LitInt i) = show i
  show (LitFloat d) = show d
  show (LitChar c) = show c
  show (LitString s) = '"' : s ++ "\""

data FloExpr = FloLit Literal                     -- Literals
             | FloFun Name [Input]                -- Functions
             | FloCons Name [Input]               -- Constructors
             | FloAp FloExpr FloExpr              -- Applications
             | FloLambda [Input] FloExpr          -- Lambdas
             | FloLet [FloDef] FloExpr            -- Let expressions
             deriving Show

data FloDef = FloDef {
  fdName :: Name,
  fdInputs :: [Input],
  fdExpr :: FloExpr
}

instance Show FloDef where
  show FloDef{..} = fdName ++ " " ++ show fdInputs ++ " = " ++ show fdExpr

data FloModule = FloModule {
  fmName :: Name,
  fmDefs :: [FloDef]
}

instance Show FloModule where
  show FloModule{..} = "FloModule " ++ fmName ++ "{\n" ++ defs' ++ "\n}"
    where defs' = intercalate "\n" $ map show fmDefs

data FloProgram = FloProgram [FloModule]

instance Show FloProgram where
  show (FloProgram modules) = intercalate "\n\n" $ map show modules

{- The following functions convert the graphical representation of a program
   into a format composed of expressions and definitions. -}
instance Convertible FloGraph FloProgram where
  convert FloGraph{..} = FloProgram $ convert modules

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
    | otherwise = case bFlavor of Function -> createAp FloFun
                                  Constructor -> createAp FloCons
                                  Literal -> convert bi
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
instance Convertible BoxInterface FloExpr where
  convert BoxInterface{..} = FloLit literal
    where literal | isLitString bName = LitString (init $ tail bName)
                  | isLitChar bName = LitChar (read bName)
                  | isLitInt bName = LitInt (read bName)
                  | isLitFloat bName = LitFloat (read bName)
                  | otherwise = error "Not valid literal"

isLitString :: String -> Bool
isLitString = (=~ "^\".*\"$")   -- ^".*"$

isLitChar :: String -> Bool
isLitChar = (=~ "^'[^\\']'$|^'\\\\['trn]'$|^'\\\\\\\\'$")
              -- ^'[^\']'$|^'\\['trn]'$|^'\\\\'$

isLitInt :: String -> Bool
isLitInt = (=~ "^[0-9]+$")    -- ^[0-9]+$

isLitFloat :: String -> Bool
isLitFloat = (=~ "^[0-9]*\\.[0-9]+$")     -- ^[0-9]*\.[0-9]+$
