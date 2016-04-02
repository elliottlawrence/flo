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

data COp = CEq | CNe | CLt | CPlus | CMinus | CMult | CDiv

{- The local environment consists of locations on the stacks, locations in the
   current closure, and dynamically allocated closures. -}
data LocalEnv = LocalEnv {
  _localAStack :: [(Var,Int)],
  _localBStack :: [(Var,Int)],
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

type Closure = CVarDecl
type InfoTable = CVarDecl
type StdEntry = CFunction

{- During compilation we generate var declarations and functions. -}
data Decls = Decls {
  _varDecls :: [CVarDecl],
  _infoTables :: [InfoTable],
  _closures :: [Closure],
  _stdEntries :: [CFunction]
}
makeLenses ''Decls

{- A reader-state monad with a read-only environment and mutable state. -}
newtype RS r s a = RS { rs :: ReaderT r (State s) a}
  deriving (Functor, Applicative, Monad, MonadReader r, MonadState s)

data St = St {
  _decls :: Decls,
  _altsNum :: Int
}
makeLenses ''St

type SEnv a = RS Env St a

liftREnv :: REnv a -> SEnv a
liftREnv r = liftM (runReader r) ask

{- Alter the type of the state for the RS monad. -}
alterState :: (t -> s) -> (s -> t -> t) -> RS Env s a -> RS Env t a
alterState getState putState m = do
  b <- get
  (m',s') <- liftM2 (runRS m) ask (gets getState)
  modify (putState s')
  return m'

runRS :: RS r s a -> r -> s -> (a, s)
runRS RS{..} = runState . runReaderT rs

evalRS :: RS r s a -> r -> s -> a
evalRS RS{..} = evalState . runReaderT rs

execRS :: RS r s a -> r -> s -> s
execRS RS{..} = execState . runReaderT rs

{- Lookup a variable. If it's in the local environment, apply the corresponding
   function to its index on the stack or heap. If not, return the default
   value. -}
lookupEnv :: Var -> Env ->
            (Int -> a) -> (Int -> a) -> (Int -> a) -> (Int-> a) -> a -> a
lookupEnv name env justSA justSB justH justL nothing =
  -- Lookup the variable in the A stack
  case lookupName localAStack of
    Just i -> justSA i
    Nothing -> -- Lookup the variable in the B stack
      case lookupName localBStack of
        Just i -> justSB i
        Nothing -> -- Lookup the variable in the current closure
          case lookupName localFree of
            Just i -> justH i
            Nothing -> -- Lookup the variable in the dynamic closures
              case lookupName dynamicFree of
                Just i -> justL i
                Nothing -> nothing
  where lookupName loc = lookup name (env^.lEnv.loc)

{- Lookup a name in the environment and convert it to an expression -}
lookupVarExpr :: Var -> Env -> CExpr
lookupVarExpr name env = lookupEnv name env
  (CID . ('a':) . show)            -- A stack
  (CID . ('b':) . show . negate)   -- B stack
  offsetNode                       -- Offset from Node
  (CCast pointerTD . offsetHeap)   -- Offset from heap pointer
  (CCast pointerTD $ CID $ name ++ "_closure")  -- Static closure

{- Lookup the unique tag associated with a data constructor. -}
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

{- Registers -}
spA, spB, hp, hLimit, node, rTag, intReg :: String
spA = "SpA"
spB = "SpB"
hp = "Hp"
hLimit = "HLimit"
node = "Node"
rTag = "RTag"
intReg = "IntReg"

-- Convenience functions for manipulating the stacks, heap, etc.

offset :: ID -> Int -> CExpr
offset name i | i == 0 = CID name
              | otherwise = COp op (CID name) (CLit $ abs i)
  where op | i > 0 = CPlus
           | otherwise = CMinus

assignStackA :: Int -> CExpr -> CStatement
assignStackA i = CSExpr . CAssign spA (Just i)

offsetStackA :: Int -> CExpr
offsetStackA = offset spA

assignStackB :: Int -> CExpr -> CStatement
assignStackB i = CSExpr . CAssign spB (Just i)

offsetStackB :: Int -> CExpr
offsetStackB = offset spB

assignHeap :: Int -> CExpr -> CStatement
assignHeap i = CSExpr . CAssign hp (Just i)

offsetHeap :: Int -> CExpr
offsetHeap = offset hp

assignNode :: CExpr -> CStatement
assignNode = CSExpr . CAssign node Nothing

offsetNode :: Int -> CExpr
offsetNode = COp CPlus (CID node) . CLit

assignRTag :: CExpr -> CStatement
assignRTag = CSExpr . CAssign rTag Nothing

assignIntReg :: CExpr -> CStatement
assignIntReg = CSExpr . CAssign intReg Nothing

{- Update node and enter it -}
updateNodeEnter :: Var -> Env -> [CStatement]
updateNodeEnter name env = [
  CAnn ("Grab " ++ name ++ " into Node") $ assignNode $ lookupVarExpr name env,
  CAnn ("Enter " ++ name) $ CSEnter $ "(pointer**)" ++ node]

{- Pop off the return address from the B stack and enter it -}
popRetEnter :: [CStatement]
popRetEnter =
  [CSExpr $ CAssign spB Nothing $ offsetStackB (-1),
   CAnn "Enter return address" $ CSEnter $ "(pointer**)" ++ spB ++ "[1]"]

{- Allocate space in the heap -}
allocateHeap :: Int -> [CStatement]
allocateHeap i = [CAnn "Allocate some heap" $
  CSExpr $ CAssign hp Nothing $ offsetHeap (negate i),
  CSIf (COp CLt (CID hp) (CID hLimit))
    [CSExpr $ CCall (CID "printf") [CString "Error: Out of heap space\\n"],
     CSExpr $ CCall (CID "exit") [CLit 0]]
    [] ]

lookupOp :: Var -> COp
lookupOp var = fromMaybe (error "Primitive operator not found") $
  lookup var [("+#",CPlus), ("-#",CMinus), ("*#",CMult), ("/#",CDiv)]
