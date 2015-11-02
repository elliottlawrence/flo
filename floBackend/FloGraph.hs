module FloGraph where

import Data.List (find)
import qualified Data.Map.Lazy as Map
import Data.Maybe (fromJust)

type Name = String

{- A type can either be an atomic, raw type, or a function from one type to
   another. -}
data Type = RawType Name | Type :-> Type

{- An input to a function has a name and a type. Outputs are unnamed but have
   types nonetheless. -}
data Input = Input {
  getInputName :: Name,
  getInputType :: Type
}
data Output = Output {
  getOutputType :: Type
}

{- A box is a generic representation of functions, constructors, and literals.
   When used in expressions, these appear as "black boxes", and only their
   interface is visible. This consists of a name, a set of inputs, and an
   output. -}
data BoxFlavor = Function | Constructor | Literal
data BoxInterface = BoxInterface {
  getBoxFlavor :: BoxFlavor,
  getBoxName :: Name,
  getBoxInputs :: [Input],
  getBoxOutput :: Output
}

getBoxType :: BoxInterface -> Type
getBoxType box = foldr1 (:->) (inputTypes ++ [outputType])
  where inputTypes = map getInputType $ getBoxInputs box
        outputType = getOutputType $ getBoxOutput box

{- Within a function definition, the same box can appear multiple times. Thus,
   boxes are identified by a unique key. -}
type ID = Int
type BoxInterfaceMap = Map.Map ID BoxInterface

{- A cable connects the output of one box to the input of another.
   Alternatively, a cable can start from a single output, indicating that the
   other end will be connected to the output of the surrounding function. -}
data Cable = ID :-: (ID, Input)
           | CableOut ID

{- A box definition consists of the box's interface augmented with a set of
   boxes and cables connecting nodes on the boxes. In addition, a box may have
   local box definitions, akin to a let/where clause in Haskell. -}
data BoxDefinition = BoxDefinition {
  getBoxInterface :: BoxInterface,
  getBoxes :: BoxInterfaceMap,
  getCables :: [Cable],
  getLocalDefinitions :: [BoxDefinition]
}

{- Get the box that is connected to a function's output. -}
getOutputBox :: BoxDefinition -> ID
getOutputBox boxDef = boxID
  where cables = getCables boxDef
        CableOut boxID = fromJust $ find isCableOut cables
        isCableOut (CableOut _) = True
        isCableOut _ = False

{- Given a box definition and a specific box, find all of its inputs. -}
getAppliedInputs :: BoxDefinition -> ID -> [(Input, ID)]
getAppliedInputs boxDef boxID = concatMap filterCables cables
  where cables = getCables boxDef
        filterCables (inputID :-: (boxID', input))
          | boxID' == boxID = [(input, inputID)]
        filterCables (_ :-: _) = []
        filterCables (CableOut _) = []

{- A module consists of a list of definitions. In turn, a flo graph is a
   graphical representation of a program, consisting of a list of modules. -}
data Module = Module {
  getModuleName :: Name,
  getModuleDefinitions :: [BoxDefinition]
}
data FloGraph = FloGraph {
  getFloGraphName :: Name,
  getFloGraphModules :: [Module]
}
