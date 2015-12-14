module FloGraph where

import qualified Data.IntMap as IntMap
import Data.List (find, intercalate)
import Data.Maybe (fromMaybe)

type Name = String

{-
data Type = RawType Name | Type :-> Type
getBoxType :: BoxInterface -> Type
getBoxType box = foldr1 (:->) (inputTypes ++ [outputType])
  where inputTypes = map getInputType $ getBoxInputs box
        outputType = getOutputType $ getBoxOutput box
-}

data Input = Input {
  getInputName :: Name,
  getInputParentID :: ID
} deriving Eq

instance Show Input where
  show (Input name id) = "Input {" ++ name ++ ", " ++ show id ++ "}"

data Output = Output {
  getOutputParentID :: ID
}

instance Show Output where
  show (Output id) = "Output {" ++ show id ++ "}"

data Cable = Output :-: Input deriving Show

data BoxFlavor = Function | Constructor | Literal deriving Show
data BoxInterface = BoxInterface {
  getBoxName :: Name,
  getBoxFlavor :: BoxFlavor,
  getBoxInputs :: [Input]
}

instance Show BoxInterface where
  show (BoxInterface name flavor inputs) = "Box {" ++ name ++ ", {" ++
    intercalate ", " (map show inputs) ++ "}}"

{- Within a function definition, the same box can appear multiple times. Thus,
   boxes are identified by a unique key. -}
type ID = Int
type BoxInterfaceMap = IntMap.IntMap BoxInterface

data BoxDefinition = BoxDefinition {
  getBoxInterface :: BoxInterface,
  getBoxes :: BoxInterfaceMap,
  getCables :: [Cable],
  getLocalDefinitions :: [BoxDefinition]
}

instance Show BoxDefinition where
  show (BoxDefinition interface boxes cables defs) = show interface ++ " = " ++
    "{\nBoxes {" ++ show boxes ++ "}\nCables {" ++ show cables ++
    "}\nDefinitions " ++ show defs ++ "}"

data Module = Module {
  getModuleName :: Name,
  getModuleDefinitions :: [BoxDefinition]
}

instance Show Module where
  show (Module name defs) = "Module " ++ name ++ "{\n" ++ defs' ++ "\n}"
    where defs' = intercalate "\n" $ map show defs

data FloGraph = FloGraph {
  getFloGraphModules :: [Module]
}

instance Show FloGraph where
  show (FloGraph modules) = intercalate "\n\n" $ map show modules

{- Get the box that is connected to a function's output. -}
getOutputBox :: BoxDefinition -> ID
getOutputBox boxDef = getOutputParentID output
  where cables = getCables boxDef
        output :-: input = fromMaybe (error "No last cable") $
                           find isLastCable cables
        isLastCable (output :-: input) = isEndInput input
        isEndInput input = getInputParentID input == -1

{- Given a box definition and a specific box, find all of its inputs. -}
getAppliedInputs :: BoxDefinition -> ID -> [(Input, ID)]
getAppliedInputs boxDef boxID = concatMap filterCables cables
  where cables = getCables boxDef
        filterCables (output :-: input)
          | getInputParentID input == boxID
            = [(input, getOutputParentID output)]
        filterCables (_ :-: _) = []
