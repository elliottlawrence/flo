module FloProgram where

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
  getDefinitionInputs :: [Input],
  getExpression :: FloExpression
}

instance Show FloDefinition where
  show (FloDefinition name inputs expr) = name ++ " " ++ show inputs ++ " = "
    ++ show expr

{- A module consists of a list of definitions. In turn, a flo program is simply
   a list of modules. -}
data FloModule = FloModule {
  getFloModuleName :: Name,
  getFloModuleDefinitions :: [FloDefinition]
}

instance Show FloModule where
  show (FloModule name defs) = "FloModule " ++ name ++ "{\n" ++ defs' ++ "\n}"
    where defs' = intercalate "\n" $ map show defs

data FloProgram = FloProgram [FloModule]

instance Show FloProgram where
  show (FloProgram modules) = intercalate "\n\n" $ map show modules

{- Given a FloExpression, finds all the inputs to it, which may be nested
   arbitrarily. -}
getInputs :: FloExpression -> [Input]
getInputs (FloLit _) = []
getInputs (FloFun _ inputs) = inputs
getInputs (FloCons _ inputs) = inputs
getInputs (FloAp expr apps) = getInputs expr \\ map fst apps
getInputs (FloLet _ expr) = getInputs expr

{- The following functions convert the graphical representation of a program
   into a format composed of expressions and definitions. -}
convertFloGraph :: FloGraph -> FloProgram
convertFloGraph fg = FloProgram $ map convertModule modules
  where modules = getFloGraphModules fg

convertModule :: Module -> FloModule
convertModule m = FloModule (getModuleName m)
  (map convertBoxDefinition $ getModuleDefinitions m)

{- The expression component of a box definition is the expression determined by
   the output box. In addition, if a box has local definitions, these are
   captured by wrapping the box definition in a let expression. -}
convertBoxDefinition :: BoxDefinition -> FloDefinition
convertBoxDefinition boxDef = FloDefinition {
  getDefinitionName = getBoxName boxInterface,
  getDefinitionInputs = getBoxInputs boxInterface,
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
