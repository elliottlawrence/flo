{-# LANGUAGE RecordWildCards, TypeSynonymInstances, FlexibleInstances #-}
module FloGraph where

import Pretty

import qualified Data.IntMap as IntMap
import Data.List (find)
import Data.Maybe (fromMaybe)
import Text.PrettyPrint.Leijen hiding (Pretty)

type Name = String

data Input = Input {
  iName :: Name,
  iParentID :: ID
} deriving Eq

data Output = Output {
  oParentID :: ID,
  oEndInputName :: Name
}

data Cable = Output :-: Input

data BoxInterface = BoxInterface {
  bName :: Name,
  bInputs :: [Input]
}

{- Within a function definition, the same box can appear multiple times. Thus,
   boxes are identified by a unique key. -}
type ID = Int
newtype BoxInterfaceMap = BoxInterfaceMap {biMap :: IntMap.IntMap BoxInterface}

data BoxDef = BoxDef {
  boxInterface :: BoxInterface,
  boxes :: BoxInterfaceMap,
  cables :: [Cable],
  localDefs :: [BoxDef]
}

data Module = Module {
  mName :: Name,
  mDefs :: [BoxDef]
}

data FloGraph = FloGraph [Module]

endInput :: Input
endInput = Input "endInput" (-1)

{- Get the output of the box that is connected to a given input. -}
getConnectedOutput :: BoxDef -> Input -> Maybe Output
getConnectedOutput BoxDef{..} i = maybe Nothing (\(o :-: _) -> Just o) $
  find (\(_ :-: i') -> i == i') cables

getConnectedOutputUnsafe :: BoxDef -> Input -> Output
getConnectedOutputUnsafe bd
  = fromMaybe (error "No connected output") . getConnectedOutput bd

{- Returns whether the given input has a cable connected to it -}
isApplied :: BoxDef -> Input -> Bool
isApplied BoxDef{..} i = any (\(_ :-: i') -> i == i') cables

{- Lookup a box with a given ID. -}
lookupUnsafe :: Int -> BoxInterfaceMap -> BoxInterface
lookupUnsafe id (BoxInterfaceMap boxes) =
  fromMaybe (error "Box not found") $ IntMap.lookup id boxes

{- Get the box interface that is connected to the given input. -}
getConnectedBox :: BoxDef -> Input -> BoxInterface
getConnectedBox bd i
  = lookupUnsafe (oParentID $ getConnectedOutputUnsafe bd i) (boxes bd)

-- Pretty printing
instance Pretty Input where
  pp Input{..} = text "Input:" <+> commas [text iName, int iParentID]

instance Pretty Output where
  pp Output{..} = text "Output:" <+> int oParentID

instance Pretty Cable where
  pp (o :-: i) = pp o <+> text ":-:" <+> pp i

instance Pretty BoxInterface where
  pp BoxInterface{..} = text "Box:" <+> text bName <$$>
    text "Inputs:" <+> align (pp bInputs)

instance Pretty BoxInterfaceMap where
  pp (BoxInterfaceMap boxes) = vcat $
    map (\(key,a) -> int key <> colon <+> pp a) (IntMap.toList boxes)

instance Pretty BoxDef where
  pp BoxDef{..} = pp boxInterface <$$>
    indent 4 (text "Boxes:" <+> pp boxes <$$>
              text "Cables:" <+> align (pp cables) <$$>
              text "Definitions:" <+> align (pp localDefs))

instance Pretty Module where
  pp Module{..} = text "Module:" <+> text mName <$$> indent 4 (pp mDefs)

instance Pretty FloGraph where
  pp (FloGraph modules) = pp modules
