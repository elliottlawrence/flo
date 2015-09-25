module FloGraph where

import qualified Data.Map.Lazy as Map

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

{- A box is a generic representation of functions, constants, and literals. When
   used in expressions, these appear as "black boxes", and only their interface
   is visible. This consists of a name, a set of inputs (which is nonempty only
   for functions), and an output. -}
data BoxInterface = BoxInterface {
  getBoxName :: Name,
  getInputs :: [Input],
  getOutput :: Output
}

getBoxType :: BoxInterface -> Type
getBoxType box = foldr1 (:->) (inputTypes ++ [outputType])
  where inputTypes = map getInputType $ getInputs box
        outputType = getOutputType $ getOutput box

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
   boxes and cables connecting nodes on the boxes. -}
data BoxDefinition = BoxDefinition {
  getBoxInterface :: BoxInterface,
  getBoxes :: BoxInterfaceMap,
  getCables :: [Cable]
}

{- A module consists of a list of definitions. In turn, a program is simply a
   list of modules. -}
type Module = [BoxDefinition]
type Program = [Module]
