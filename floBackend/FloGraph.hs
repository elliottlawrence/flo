{-# LANGUAGE RecordWildCards #-}
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
  iName :: Name,
  iParentID :: ID
} deriving Eq

instance Show Input where
  show Input{..} = "Input {" ++ iName ++ ", " ++ show iParentID ++ "}"

data Output = Output {
  oParentID :: ID
}

instance Show Output where
  show Output{..} = "Output {" ++ show oParentID ++ "}"

data Cable = Output :-: Input deriving Show

data BoxFlavor = Function | Constructor | Literal deriving Show
data BoxInterface = BoxInterface {
  bName :: Name,
  bFlavor :: BoxFlavor,
  bInputs :: [Input]
}

instance Show BoxInterface where
  show BoxInterface{..} = "Box {" ++ bName ++ ", {" ++
    intercalate ", " (map show bInputs) ++ "}}"

{- Within a function definition, the same box can appear multiple times. Thus,
   boxes are identified by a unique key. -}
type ID = Int
type BoxInterfaceMap = IntMap.IntMap BoxInterface

data BoxDef = BoxDef {
  boxInterface :: BoxInterface,
  boxes :: BoxInterfaceMap,
  cables :: [Cable],
  localDefs :: [BoxDef]
}

instance Show BoxDef where
  show BoxDef{..} = show boxInterface ++ " = " ++ "{\nBoxes {" ++
    show boxes ++ "}\nCables {" ++ show cables ++
    "}\nDefinitions " ++ show localDefs ++ "}"

data Module = Module {
  mName :: Name,
  mDefs :: [BoxDef]
}

instance Show Module where
  show Module{..} = "Module " ++ mName ++ "{\n" ++ defs' ++ "\n}"
    where defs' = intercalate "\n" $ map show mDefs

data FloGraph = FloGraph {
  modules :: [Module]
}

instance Show FloGraph where
  show FloGraph{..} = intercalate "\n\n" $ map show modules

{- Get the box that is connected to a function's output. -}
getOutputBox :: BoxDef -> ID
getOutputBox BoxDef{..} = oParentID
  where Output{..} :-: input = fromMaybe (error "No last cable") $
                               find isLastCable cables
        isLastCable (output :-: Input{..}) = iParentID == -1

{- Given a box definition and a specific box, find all of its inputs. -}
getAppliedInputs :: BoxDef -> ID -> [(Input, ID)]
getAppliedInputs BoxDef{..} boxID = concatMap filterCables cables
  where filterCables (Output{..} :-: i@Input{..})
          | iParentID == boxID = [(i, oParentID)]
        filterCables (_ :-: _) = []
