module FloGraph where

import qualified Data.IntMap as IntMap
import Data.List (find)
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
} deriving Show

data Output = Output {
  getOutputParentID :: ID
} deriving Show

data Cable = Output :-: Input deriving Show

data BoxFlavor = Function | Constructor | Literal deriving Show
data BoxInterface = BoxInterface {
  getBoxName :: Name,
  getBoxFlavor :: BoxFlavor,
  getBoxInputs :: [Input]
} deriving Show

{- Within a function definition, the same box can appear multiple times. Thus,
   boxes are identified by a unique key. -}
type ID = Int
type BoxInterfaceMap = IntMap.IntMap BoxInterface

data BoxDefinition = BoxDefinition {
  getBoxInterface :: BoxInterface,
  getBoxes :: BoxInterfaceMap,
  getCables :: [Cable],
  getLocalDefinitions :: [BoxDefinition]
} deriving Show

data Module = Module {
  getModuleName :: Name,
  getModuleDefinitions :: [BoxDefinition]
} deriving Show

data FloGraph = FloGraph {
  getFloGraphModules :: [Module]
} deriving Show

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
