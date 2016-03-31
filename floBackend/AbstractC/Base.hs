{-# LANGUAGE TemplateHaskell, GeneralizedNewtypeDeriving, RecordWildCards #-}
module AbstractC.Base where

import STG

import Control.Monad.Reader
import Control.Monad.State
import Data.Maybe (fromMaybe)
import Lens.Micro
import Lens.Micro.TH

type ID = String

data CProgram = CProgram [CDeclString] [CVarDecl] [CFunction]

{- Top-level declarations can be complicated, so we'll just say they're
   strings.-}
type CDeclString = String

data CVarDecl = CVarDecl {
  varType :: CType,
  varName :: ID,
  varArrSize :: Maybe Int,
  varExpr :: Maybe CExpr
}

data CFunction = CFunction {
  funType :: CType,
  funName :: ID,
  funArgs :: [CParam],
  funBody :: [CStatement]
}

data CType = CInt | CTypeDef String | CPointerType CType

data CParam = CParam CType ID

data CStatement = CSVarDecl CVarDecl
                | CSExpr CExpr
                | CSIf CExpr [CStatement] [CStatement]
                | CSWhile CExpr [CStatement]
                | CSwitch CExpr [CCase]
                | CSReturn (Maybe CExpr)
                | CSEnter ID
                | CJump ID
                | CComment String                     -- Comments
                | CAnn String CStatement              -- Statement with comment

data CCase = CCase Int [CStatement]

data CExpr = CID ID
           | CAssign ID (Maybe Int) CExpr
           | COp COp CExpr CExpr
           | CLit Lit
           | CString String
           | CPointer ID
           | CCall CExpr [CExpr]
           | CCast CType CExpr
           | CArray [CExpr]
           | CArrayElement ID Int
           | CParens CExpr

data COp = CEq | CNe | CLt | CPlus | CMinus

{- The local environment consists of locations on the stack, locations in the
   current closure, and dynamically allocated closures. -}
data LocalEnv = LocalEnv {
  _localStack :: [(Var,Int)],
  _localFree :: [(Var,Int)],
  _dynamicFree :: [(Var,Int)]
}
makeLenses ''LocalEnv

{- The global environment consists of the top-level bindings and data
   constructors. -}
data GlobalEnv = GlobalEnv {
  _globs :: Bindings,
  _dataConses :: [(Cons,Int)]
}
makeLenses ''GlobalEnv

data Env = Env {
  _lEnv :: LocalEnv,
  _gEnv :: GlobalEnv
}
makeLenses ''Env

type REnv a = Reader Env a

{- During compilation we may need to generate new functions and variables. -}
data ExtraDecls = ExtraDecls {
  _extraVarDecls :: [CVarDecl],
  _extraFuns :: [CFunction]
}
makeLenses ''ExtraDecls

{- A reader-state monad with a read-only environment and mutable state. -}
newtype RS r s a = RS { rs :: ReaderT r (State s) a}
  deriving (Functor, Applicative, Monad, MonadReader r, MonadState s)

data St = St {
  _extraDecls :: ExtraDecls,
  _altsNum :: Int
}
makeLenses ''St

type SEnv a = RS Env St a

liftREnv :: REnv a -> SEnv a
liftREnv r = liftM (runReader r) ask

runRS :: RS r s a -> r -> s -> (a, s)
runRS RS{..} = runState . runReaderT rs

evalRS :: RS r s a -> r -> s -> a
evalRS RS{..} = evalState . runReaderT rs

{- Lookup a variable. If it's in the local environment, apply the corresponding
   function to its index on the stack or heap. If not, return the default
   value. -}
lookupEnv :: Var -> Env -> (Int -> a) -> (Int -> a) -> (Int-> a) -> a -> a
lookupEnv name env justS justH justL nothing =
  case lookup name (env^.lEnv.localStack) of
    Just i -> justS i
    Nothing -> case lookup name (env^.lEnv.localFree) of
                 Just i -> justH i
                 Nothing -> case lookup name (env^.lEnv.dynamicFree) of
                              Just i -> justL i
                              Nothing -> nothing

lookupTag :: Cons -> Env -> Int
lookupTag cons env = fromMaybe (error "Data constructor not found") $
  lookup cons (env ^. gEnv . dataConses)

{- Adjusts all the heap offsets for dynamically allocated closures. Useful when
   the heap pointer has changed but we still need to reference these closures.
   -}
adjustDynamicFree :: Int -> [(Var,Int)] -> [(Var,Int)]
adjustDynamicFree i free = [ (var,index + i) | (var,index) <- free]

pointerTD :: CType
pointerTD = CTypeDef "pointer"

functionTD :: CType
functionTD = CTypeDef "function"

createFunPrototype :: CFunction -> CVarDecl
createFunPrototype CFunction{..} = CVarDecl funType (funName ++ "()")
  Nothing Nothing

spA, spB, hp, hLimit, node, rTag :: String
spA = "SpA"
spB = "SpB"
hp = "Hp"
hLimit = "HLimit"
node = "Node"
rTag = "RTag"

-- Convenience functions for manipulating the stacks, heap, etc.

offset :: ID -> Int -> CExpr
offset name i | i == 0 = CID name
              | otherwise = COp op (CID name) (CLit $ abs i)
  where op | i > 0 = CPlus
           | otherwise = CMinus

assignStackA :: Int -> CExpr -> CStatement
assignStackA i e = CSExpr $ CAssign spA (Just i) e

offsetStackA :: Int -> CExpr
offsetStackA = offset spA

assignStackB :: Int -> CExpr -> CStatement
assignStackB i e = CSExpr $ CAssign spB (Just i) e

offsetStackB :: Int -> CExpr
offsetStackB = offset spB

assignHeap :: Int -> CExpr -> CStatement
assignHeap i e = CSExpr $ CAssign hp (Just i) e

offsetHeap :: Int -> CExpr
offsetHeap = offset hp

assignNode :: CExpr -> CStatement
assignNode e = CSExpr $ CAssign node Nothing e

offsetNode :: Int -> CExpr
offsetNode = COp CPlus (CID node) . CLit

assignRTag :: CExpr -> CStatement
assignRTag e = CSExpr $ CAssign rTag Nothing e

{- Allocate space in the heap -}
allocateHeap :: Int -> [CStatement]
allocateHeap i = [CAnn "Allocate some heap" $
  CSExpr $ CAssign hp Nothing $ offsetHeap (negate i),
  CSIf (COp CLt (CID hp) (CID hLimit))
    [CSExpr $ CCall (CID "printf") [CString "Error: Out of heap space\\n"],
     CSExpr $ CCall (CID "exit") [CLit 0]]
    [] ]
