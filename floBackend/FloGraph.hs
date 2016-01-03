{-# LANGUAGE RecordWildCards #-}
module FloGraph where

import qualified Data.IntMap as IntMap
import Data.List ((\\), find)
import Data.Maybe (fromMaybe)
import Text.PrettyPrint

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

data Output = Output {
  oParentID :: ID,
  oEndInputName :: Name
}

data Cable = Output :-: Input deriving Show

data BoxFlavor = Function | Constructor | Literal deriving Show
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

data FloGraph = FloGraph {
  modules :: [Module]
}

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
commas :: [Doc] -> Doc
commas = hsep . punctuate comma

instance Show Input where
  show = render . ppInput

ppInput :: Input -> Doc
ppInput Input{..} = text "Input:" <+> commas [text iName, int iParentID]

instance Show Output where
  show = render . ppOutput

ppOutput :: Output -> Doc
ppOutput Output{..} = text "Output:" <+> int oParentID

instance Show BoxInterface where
  show = render . ppBoxInterface

ppBoxInterface :: BoxInterface -> Doc
ppBoxInterface BoxInterface{..} = text "Box:" <+> text bName $$
  text "Inputs:" <+> commas (map ppInput bInputs)

instance Show BoxDef where
  show = render . ppBoxDef

ppBoxDef :: BoxDef -> Doc
ppBoxDef BoxDef{..} = ppBoxInterface boxInterface <+> equals $+$
  nest 4 (text "Boxes:" <+> text (show boxes) $+$
          text "Cables:" <+> commas (map (text . show) cables) $+$
          text "Definitions:" <+> cat (map ppBoxDef localDefs))

instance Show Module where
  show = render . ppModule

ppModule :: Module -> Doc
ppModule Module{..} = text "Module:" <+> text mName $+$
  nest 4 (vcat $ map ppBoxDef mDefs)

instance Show FloGraph where
  show = render . ppFloGraph

ppFloGraph :: FloGraph -> Doc
ppFloGraph FloGraph{..} = vcat $ map ppModule modules
