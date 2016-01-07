{-# LANGUAGE RecordWildCards, TypeSynonymInstances, FlexibleInstances #-}
module FloGraph where

import Pretty

import qualified Data.IntMap as IntMap
import Data.List ((\\), find)
import Data.Maybe (fromMaybe)
import Text.PrettyPrint

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

data BoxFlavor = Function | Literal
data BoxInterface = BoxInterface {
  bName :: Name,
  bFlavor :: BoxFlavor,
  bInputs :: [Input]
}

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

data Module = Module {
  mName :: Name,
  mDefs :: [BoxDef]
}

data FloGraph = FloGraph [Module]

{- Get the output of the box that is connected to a function's output. -}
getOutput :: BoxDef -> Output
getOutput BoxDef{..} = output
  where output :-: _ = fromMaybe (error "No last cable") $
                       find isLastCable cables
        isLastCable (_ :-: Input{..}) = iParentID == -1

{- Given a box definition and a specific box, find all of its applied inputs. -}
getAppliedInputs :: BoxDef -> ID -> [(Input, Output)]
getAppliedInputs BoxDef{..} boxID = concatMap filterCables cables
  where filterCables (o :-: i@Input{..})
          | iParentID == boxID = [(i, o)]
        filterCables (_ :-: _) = []

{- Given a box definition and a specific box, find all of its unapplied
   inputs. -}
getUnappliedInputs :: BoxDef -> ID -> [Input]
getUnappliedInputs bd@BoxDef{..} boxID = bInputs \\ appliedInputs
  where BoxInterface{..} = fromMaybe (error "Box not found") $
                           IntMap.lookup boxID boxes
        appliedInputs = map fst $ getAppliedInputs bd boxID

-- Pretty printing
instance Pretty Input where
  pp Input{..} = text "Input:" <+> commas [text iName, int iParentID]

instance Pretty Output where
  pp Output{..} = text "Output:" <+> int oParentID

instance Pretty Cable where
  pp (o :-: i) = pp o <+> text ":-:" <+> pp i

instance Pretty BoxInterface where
  pp BoxInterface{..} = text "Box:" <+> text bName $$
    text "Inputs:" <+> vcat (map pp bInputs)

instance Pretty BoxInterfaceMap where
  pp biMap = vcat $ map (\(key,a) -> int key <> colon <+> pp a)
    (IntMap.toList biMap)

instance Pretty BoxDef where
  pp BoxDef{..} = pp boxInterface <+> equals $+$
    nest 4 (text "Boxes:" <+> pp boxes $+$
            text "Cables:" <+> vcat (map pp cables) $+$
            text "Definitions:" <+> cat (map pp localDefs))

instance Pretty Module where
  pp Module{..} = text "Module:" <+> text mName $+$
    nest 4 (vcat $ map pp mDefs)

instance Pretty FloGraph where
  pp (FloGraph modules) = vcat . map pp $ modules
