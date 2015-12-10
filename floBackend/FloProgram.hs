module FloProgram where

import FloGraph

import Control.Arrow (second)
import qualified Data.IntMap as IntMap
import Data.Maybe (fromMaybe)
import Text.Regex.Posix

{- These are the primitive types that are supported by Flo. -}
data Literal = LitInt Int | LitFloat Double | LitChar Char | LitString String
               deriving Show

{- Expressions can be literals, functions, constructors, applications, or let
   expressions. -}
data FloExpression = FloLit Literal                     -- Literal
                   | FloFun Name [Input]                -- Functions
                   | FloCons Name [Input]               -- Constructors
                   | FloAp FloExpression [(Input, FloExpression)]
                                                        -- Applications
                   | FloLet {                           -- Let expressions
                     getLetDefinitions :: [FloDefinition],
                     getLetExpression :: FloExpression
                   } deriving Show

{- A function definition consists of a name, a list of inputs, and a defining
   expression. -}
data FloDefinition = FloDefinition {
  getDefinitionName :: Name,
  getInputs :: [Input],
  getExpression :: FloExpression
} deriving Show

{- A module consists of a list of definitions. In turn, a flo program is simply
   a list of modules. -}
data FloModule = FloModule {
  getFloModuleName :: Name,
  getFloModuleDefinitions :: [FloDefinition]
} deriving Show

data FloProgram = FloProgram [FloModule] deriving Show

{- The following functions convert the graphical representation of a program
   into a format composed of expressions and definitions. -}
convertFloGraph :: FloGraph -> FloProgram
convertFloGraph fg = FloProgram $ map convertModule modules
  where modules = getFloGraphModules fg

convertModule :: Module -> FloModule
convertModule m = FloModule {
  getFloModuleName = name,
  getFloModuleDefinitions = map convertBoxDefinition defs }
  where name = getModuleName m
        defs = getModuleDefinitions m

{- The expression component of a box definition is the expression determined by
   the output box. In addition, if a box has local definitions, these are
   captured by wrapping the box definition in a let expression. -}
convertBoxDefinition :: BoxDefinition -> FloDefinition
convertBoxDefinition boxDef = FloDefinition {
  getDefinitionName = getBoxName boxInterface,
  getInputs = getBoxInputs boxInterface,
  getExpression =
    if null localDefinitions then boxExpression
    else FloLet {
      getLetDefinitions = map convertBoxDefinition localDefinitions,
      getLetExpression = boxExpression
    }
  }
  where boxInterface = getBoxInterface boxDef
        outputBoxID = getOutputBox boxDef
        localDefinitions = getLocalDefinitions boxDef
        boxExpression = createFloExpression boxDef outputBoxID

createFloExpression :: BoxDefinition -> ID -> FloExpression
createFloExpression boxDef boxID =
  case boxFlavor of
    Function -> createApplication boxDef boxID (FloFun boxName inputs)
    Constructor -> createApplication boxDef boxID (FloCons boxName inputs)
    Literal -> convertLiteral boxInterface
  where boxInterface = fromMaybe (error "Box not found") $
                        IntMap.lookup boxID (getBoxes boxDef)
        boxFlavor = getBoxFlavor boxInterface
        boxName = getBoxName boxInterface
        inputs = getBoxInputs boxInterface

{- To create an expression out of a function or constructor, convert all of its
   inputs to expressions and then apply them to the function. For constant
   applicative forms, no inputs need be applied. -}
createApplication :: BoxDefinition -> ID -> FloExpression -> FloExpression
createApplication boxDef boxID leftSide
  | null appliedInputs = leftSide
  | otherwise = FloAp leftSide appliedExpressions
  where appliedInputs = getAppliedInputs boxDef boxID
        appliedExpressions =
          map (second $ createFloExpression boxDef) appliedInputs

{- A literal is simply the box's name. -}
convertLiteral :: BoxInterface -> FloExpression
convertLiteral boxInterface = FloLit literal
  where boxName = getBoxName boxInterface
        literal | isLitString boxName = LitString (init $ tail boxName)
                | isLitChar boxName = LitChar (read boxName)
                | isLitInt boxName = LitInt (read boxName)
                | isLitFloat boxName = LitFloat (read boxName)
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
