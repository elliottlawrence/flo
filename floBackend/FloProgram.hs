{-# LANGUAGE RecordWildCards, MultiParamTypeClasses, FlexibleInstances,
    TupleSections #-}
module FloProgram where

import Convertible
import FloGraph

import Control.Arrow (second)
import qualified Data.IntMap as IntMap
import Data.List ((\\), intercalate)
import Data.Maybe (fromMaybe)
import Text.Regex.Posix

{- These are the primitive types that are supported by Flo. -}
data Literal = LitInt Int | LitFloat Double | LitChar Char | LitString String

instance Show Literal where
  show (LitInt i) = show i
  show (LitFloat d) = show d
  show (LitChar c) = show c
  show (LitString s) = '"' : s ++ "\""

{- Expressions can be literals, functions, constructors, applications, or let
   expressions. -}
data FloExpr = FloLit Literal                     -- Literals
             | FloFun Name [Input]                -- Functions
             | FloCons Name [Input]               -- Constructors
             | FloAp FloExpr [(Input, FloExpr)]   -- Applications
             | FloLet [FloDef] FloExpr            -- Let expressions
             deriving Show

{- A function definition consists of a name, a list of inputs, and a defining
   expression. -}
data FloDef = FloDef {
  fdName :: Name,
  fdInputs :: [Input],
  fdExpr :: FloExpr
}

instance Show FloDef where
  show FloDef{..} = fdName ++ " " ++ show fdInputs ++ " = " ++ show fdExpr

{- A module consists of a list of definitions. In turn, a flo program is simply
   a list of modules. -}
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

{- Given a FloExpression, finds all the inputs to it, which may be nested
   arbitrarily. -}
getInputs :: FloExpr -> [Input]
getInputs (FloLit _) = []
getInputs (FloFun _ inputs) = inputs
getInputs (FloCons _ inputs) = inputs
getInputs (FloAp expr apps) = getInputs expr \\ map fst apps
getInputs (FloLet _ expr) = getInputs expr

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
          boxExpr = convert (bd, getOutputBox bd)

{- Converts the box with the given ID, defined in the given box definition, to
   a Flo Expression. -}
instance Convertible (BoxDef, ID) FloExpr where
  convert (bd@BoxDef{..}, boxID) =
    case bFlavor of Function -> createFun FloFun
                    Constructor -> createFun FloCons
                    Literal -> convert bi
    where bi@BoxInterface{..} = fromMaybe (error "Box not found") $
                                IntMap.lookup boxID boxes
          {- To create an expression out of a function or constructor, convert
             all of its inputs to expressions and then apply them to the
             function. For constant applicative forms, no inputs need be
             applied. -}
          createFun constructor | null appliedInputs = leftSide
                                | otherwise = FloAp leftSide appliedExprs
            where appliedInputs = getAppliedInputs bd boxID
                  appliedExprs = map (second $ convert . (bd,)) appliedInputs
                  leftSide = constructor bName bInputs

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
