{-# LANGUAGE MultiParamTypeClasses, RecordWildCards, TypeSynonymInstances,
    FlexibleInstances, FlexibleContexts, GeneralizedNewtypeDeriving,
    TemplateHaskell #-}
module AbstractC where

import Convertible
import Pretty
import STG

import Control.Arrow (first, second, (***))
import Control.Monad.Reader
import Control.Monad.State
import Data.Maybe (catMaybes, fromMaybe)
import qualified Data.Set as Set
import Lens.Micro
import Lens.Micro.TH
import Text.PrettyPrint.Leijen hiding (Pretty)

import Debug.Trace

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

pointerTD :: CType
pointerTD = CTypeDef "pointer"

functionTD :: CType
functionTD = CTypeDef "function"

spA, spB, hp, hLimit, node, rTag :: String
spA = "SpA"
spB = "SpB"
hp = "Hp"
hLimit = "HLimit"
node = "Node"
rTag = "RTag"

{- C doesn't allow function names to have symbols in them. -}
fixName :: String -> String
fixName = id

cMain :: CFunction
cMain = CFunction CInt "main" [] [f_main, cont, interpreter, ret]
  where f_main = CSVarDecl $ CVarDecl functionTD "f_main" Nothing
          (Just $ CCast functionTD (CID "main"))
        cont = CSVarDecl $ CVarDecl functionTD "cont" Nothing
          (Just $ CID "main_entry")
        interpreter =  CSWhile (COp CNe (CID "cont") (CID "f_main"))
          [CSExpr $ CAssign "cont" Nothing $
           CCast functionTD (CCall (CParens $ CPointer "cont") [])]
        ret = CSReturn $ Just $ CLit 0

instance Convertible STGProgram CProgram where
  convert prog = CProgram
    [stdio, stdlib, enterMacro, jumpMacro, pointerTypeDef, functionTypeDef]
    ([stack, spBDecl, spADecl, heap, hpDecl, hLimitDecl, nodeDecl, rTagDecl] ++
      prototypes ++ infoTables ++ closures ++ extras^.extraVarDecls)
    (stdEntries ++ extras^.extraFuns ++ [cMain])
    where -- After the conversion, add qualified names to all let-bound
          -- variables so there are no name clashes when we lift the definitions
          -- later
          STGProgram{..} = addPrefixes prog
          globs = Set.fromList $ map (\(STGBinding name _) -> name) stgBindings

          -- Random declarations
          stdio = "#include <stdio.h>"
          stdlib = "#include <stdlib.h>"
          enterMacro = "#define ENTER(c)  JUMP(**c)"
          jumpMacro = "#define JUMP(lbl)  return((pointer) lbl)"
          pointerTypeDef = "typedef int * pointer;"
          functionTypeDef = "typedef pointer (* function)();"
          -- Stacks
          stackLimit = 10000
          stack = CVarDecl pointerTD "Stack" (Just stackLimit) Nothing
          spBDecl = CVarDecl (CPointerType pointerTD) spB Nothing
            (Just $ CID "Stack")
          spADecl = CVarDecl (CPointerType pointerTD) spA Nothing
            (Just $ COp CPlus (CID "Stack") (CLit $ stackLimit - 1))
          -- Heap
          heapLimit = 10000
          heap = CVarDecl pointerTD "Heap" (Just heapLimit) Nothing
          hpDecl = CVarDecl (CPointerType pointerTD) hp Nothing
            (Just $ COp CPlus (CID "Heap") (CLit $ heapLimit - 1))
          hLimitDecl = CVarDecl (CPointerType pointerTD) hLimit Nothing
            (Just $ CID "Heap")
          -- Registers
          nodeDecl = CVarDecl pointerTD node Nothing Nothing
          rTagDecl = CVarDecl CInt rTag Nothing Nothing

          -- Static closures
          -- Tag the data constructors with unique numbers
          dataConses = zipWith (\(STGDataCons cons _) i -> (cons,i))
                       stgDataConses [1..]
          initialEnv = Env (LocalEnv [] [] []) (GlobalEnv globs dataConses)

          emptyExtraDecls = ExtraDecls [] []
          initialState = St emptyExtraDecls 1

          ((closures, infoTables, stdEntries),finalState) = first unzip3 $
            runRS (mapM convert stgBindings) initialEnv initialState
          extras = finalState ^. extraDecls
          prototypes = map createFunPrototype
            (cMain:stdEntries ++ extras^.extraFuns)

createFunPrototype :: CFunction -> CVarDecl
createFunPrototype CFunction{..} = CVarDecl funType (funName ++ "()")
  Nothing Nothing

type Closure = CVarDecl
type InfoTable = CVarDecl
type StdEntry = CFunction

{- Converts top-level bindings with static closures -}
instance Convertible STGBinding (SEnv (Closure, InfoTable, StdEntry)) where
  convert b@(STGBinding name lForm) = do
    (infoTable, stdEntry) <- convert b
    let closure = createClosure name infoTable
    return (closure, infoTable, stdEntry)

{- Converts local bindings which have dynamically allocated closures -}
instance Convertible STGBinding (SEnv (InfoTable, StdEntry)) where
  convert (STGBinding name lForm) = do
    stdEntry <- convert (name, lForm)
    let infoTable = createInfoTable name stdEntry
    return (infoTable, stdEntry)

createClosure :: ID -> InfoTable -> CVarDecl
createClosure name CVarDecl{..} = CVarDecl pointerTD (name ++ "_closure")
  (Just 0) (Just $ CArray [CCast pointerTD $ CID varName])

createInfoTable :: ID -> CFunction -> CVarDecl
createInfoTable name CFunction{..} = CVarDecl pointerTD (name ++ "_info")
  (Just 0) (Just $ CArray [CCast pointerTD $ CID funName])

{- Adjusts all the heap offsets for dynamically allocated closures. Useful when
   the heap pointer has changed but we still need to reference these closures.
   -}
adjustDynamicFree :: Int -> [(Var,Int)] -> [(Var,Int)]
adjustDynamicFree i free = [ (var,index + i) | (var,index) <- free]

instance Convertible (Var, LambdaForm) (SEnv CFunction) where
  convert (name, LambdaForm{..}) = do
    env <- ask
        -- References to dynamically-allocated closures are only valid within
        -- a single instruction sequence
    let env' = env & lEnv .~ LocalEnv (zip args [0..])
                                      (zip (Set.toList fVars) [1..]) []
        -- Todo: Implement these
        argSatCheck = []
        stackOverflowCheck = []
        heapOverflowCheck = []
        makeUpdateFrame = []
    expr' <- local (const env') (convert expr)
    return $ CFunction pointerTD (name ++ "_entry") [] $
      argSatCheck ++ stackOverflowCheck ++ heapOverflowCheck ++ makeUpdateFrame
      ++ expr'

instance Convertible STGExpr (SEnv [CStatement]) where
  convert e = case e of
    STGAp name args -> liftREnv $ compileAp name args
    STGLet _ binds expr -> compileLet binds expr
    STGCase expr alts -> compileCase expr alts
    STGCons cons args -> liftREnv $ compileCons cons args
    _ -> return [CJump "main"]

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

{- Compiles function applications -}
compileAp :: Var -> [Atom] -> REnv [CStatement]
compileAp name as = do
  env <- ask
  let
    -- Update node with the address of the function to call
    updateNode = CAnn ("Grab " ++ name ++ " into Node") $
      assignNode $ lookupVarExpr name
    enter = CAnn ("Enter " ++ name) $ CSEnter $ "(pointer**)" ++ node

    -- Lookup a name in the environment and convert it to an expression
    lookupVarExpr name = lookupEnv name env
      (CID . ("t" ++) . show) offsetNode (CCast pointerTD . offsetHeap)
      (CCast pointerTD $ CID $ name ++ "_closure")

    -- Grab all of the arguments into local variables.
    grabArgs = map grabArg (env^.lEnv.localStack)

    grabArg :: (Var,Int) -> CStatement
    grabArg (arg,i) = CAnn ("Grab " ++ arg ++ " into a local variable") $
      CSVarDecl $ CVarDecl pointerTD ("t" ++ show i)
      Nothing (Just $ CArrayElement spA i)

    -- Partition the arguments to the new function into pointers and non-
    -- pointers
    (aVars, aLits) = reverse *** reverse $ partArgs as ([],[])
      where partArgs [] (vars,lits) = (vars,lits)
            partArgs (AtomVar var:atoms) (vars,lits) =
              partArgs atoms (var:vars,lits)
            partArgs (AtomLit lit:atoms) (vars,lits) =
              partArgs atoms (vars,lit:lits)

    -- Push the arguments to the new function call on the stacks
    pushArgs = zipWith pushVar (reverse aVars) [1..] ++
               zipWith pushLit aLits [1..]

    pushVar :: Var -> Int -> CStatement
    pushVar var n = CAnn ("Push " ++ var ++ " onto stack") $
      assignStackA (length (env^.lEnv.localStack) - n) $ lookupVarExpr var

    pushLit :: Lit -> Int -> CStatement
    pushLit lit n = CAnn ("Push " ++ show lit ++ " onto stack") $
      assignStackB n (CCast pointerTD $ CLit lit)

    -- The number of arguments to be pushed onto the A and B stacks
    numA = foldl (\n a -> case a of AtomVar _ -> n + 1
                                    AtomLit _ -> n) 0 as
    numB = length as - numA

    -- Adjust the stack pointers
    adjustSp = adjust spA diffA ++ adjust spB diffB
    adjust name diff
      | diff == 0 = []
      | otherwise = [CAnn ("Adjust " ++ name) $
                     CSExpr $ CAssign name Nothing $
                     offset name diff]
    diffA = length (env^.lEnv.localStack) - numA
    diffB = numB

  return $ grabArgs ++ pushArgs ++ adjustSp ++ [updateNode] ++ [enter]

{- Allocate space in the heap -}
allocateHeap :: Int -> [CStatement]
allocateHeap i = [CAnn "Allocate some heap" $
  CSExpr $ CAssign hp Nothing $ offsetHeap (negate i),
  CSIf (COp CLt (CID hp) (CID hLimit))
    [CSExpr $ CCall (CID "printf") [CString "Error: Out of heap space\\n"],
     CSExpr $ CCall (CID "exit") [CLit 0]]
    [] ]

{- Compiles let(rec) expressions -}
compileLet :: [STGBinding] -> STGExpr -> SEnv [CStatement]
compileLet binds expr = do
  env@Env{..} <- ask
  s <- get
  let
    -- Fill in the closures, allowing for recursive definitions
    (bindsAssigns, (spaceNeeded,s')) = runState (mapM fillClosure binds) (0,s)
    (newBinds,fillClosures) = second concat $ unzip bindsAssigns

    -- The updated environment, with the new bindings from the let expression
    env' = env & lEnv.dynamicFree .~
      adjustDynamicFree spaceNeeded (env^.lEnv.dynamicFree) ++ newBinds

    -- Fill in a heap-allocated closure
    fillClosure :: STGBinding -> State (Int,St) ((Var,Int),[CStatement])
    fillClosure bind@(STGBinding name LambdaForm{..}) = do
      -- Get the current heap offset
      (i,st) <- get

      let (infoTable,stdEntry) = evalRS (convert bind) env s
          assignInfoTable = assignHeap i
            (CCast pointerTD $ CID $ varName infoTable)

          fVarE fVar = lookupEnv fVar env'
            (CArrayElement spA) offsetNode (CCast pointerTD . offsetHeap)
            (CCast pointerTD $ CID $ fVar ++ "_closure")
          assigns = assignInfoTable :
            zipWith (\i' fVar -> CAnn fVar $ assignHeap i' (fVarE fVar))
            [i+1..] (Set.toList fVars)

          comment = CComment $ "Fill in closure for " ++ name

          extras = st^.extraDecls
          extras' = extras & extraVarDecls .~ infoTable : extras^.extraVarDecls
                           & extraFuns .~ stdEntry : extras^.extraFuns
      -- Update the heap offset and extra declarations
      put (i + length assigns, st & extraDecls .~ extras')

      return ((name, i), comment:assigns)
  put s'
  expr' <- local (const env') (convert expr)
      -- Call the expression in the body of the let
  let callExpr = CComment "Evaluate body" : expr'

  return $ allocateHeap spaceNeeded ++ fillClosures ++ callExpr

{- Compiles case expressions -}
compileCase :: STGExpr -> STGAlts -> SEnv [CStatement]
compileCase e alts = do
  env <- ask
  num <- gets (^.altsNum)
  -- Increase the number of alternatives
  modify (& altsNum .~ num + 1)

  -- Compile the expression and alternatives
  e' <- convert e
  alts' <- compileAlts alts

  let saveLocalEnv = evalRS (saveFree alts) env 0
      altName = "alt" ++ show num
      pushRet = [assignStackB 1 $ CCast pointerTD $ CID altName,
                 CSExpr $ CAssign spB Nothing $ offsetStackB 1]
      callE = CComment "Evaluate body" : e'
      switchAlts = CFunction pointerTD altName [] [alts']

  -- Append the alternatives to the list of functions
  modify (& extraDecls.extraFuns %~ (switchAlts :))
  return $ saveLocalEnv ++ pushRet ++ callE

compileAlts :: STGAlts -> SEnv CStatement
compileAlts alts = do
  env <- ask
  let
      cases = case alts of
        STGAAlts aalts maybeDalt -> do
          aalts' <- mapM makeCaseA aalts
          dalt' <- maybeCase maybeDalt
          return $ aalts' ++ dalt'
        STGPAlts palts maybeDalt -> do
          palts' <- mapM makeCaseP palts
          dalt' <- maybeCase maybeDalt
          return $ palts' ++ dalt'

      maybeCase (Just (STGDAlt maybeVar e)) = do
        e' <- convert e
        return [CCase (-1) e']
      maybeCase Nothing = return []

      makeCaseA (STGAAlt cons vars e) =
        liftM (CCase (lookupTag cons env)) (convert e)
      makeCaseP (STGPAlt lit e) = liftM (CCase lit) (convert e)

  liftM (CSwitch (CID rTag)) cases

{- Compiles data constructors -}
compileCons :: Cons -> [Atom] -> REnv [CStatement]
compileCons cons args = do
  env <- ask
  let -- Allocate a closure in the heap for the constructor
      alloc = allocateHeap $ 1 + length args

      assignInfoTable = assignHeap 0 (CCast pointerTD $ CID $ cons ++ "_info")
      fVarE fVar = lookupEnv fVar env
        (CArrayElement spA) offsetNode (CCast pointerTD . offsetHeap)
        (CCast pointerTD $ CID $ fVar ++ "_closure")
      assigns = assignInfoTable :
        zipWith (\i' arg -> case arg of
          AtomVar var -> CAnn var $ assignHeap i' (fVarE var)
          AtomLit lit -> CAnn (show lit) $
                         assignHeap i' (CCast pointerTD $ CLit lit))
        [1..] args
      comment = CComment $ "Fill in closure for " ++ cons

      -- Fill in the closure
      fillClosure = comment:assigns

      -- Update RTag with the tag of the constructor
      updateRTag = assignRTag $ CLit $ lookupTag cons env
      -- Make node point to the current closure
      updateNode = CAnn ("Grab " ++ cons ++ " closure into Node") $
        assignNode $ CCast pointerTD $ CID hp
      -- Pop off the return vector and enter it
      adjustSpB = CSExpr $ CAssign spB Nothing $ offsetStackB (-1)
      enter = CAnn "Enter return address" $ CSEnter $ "(pointer**)" ++ spB ++ "[1]"

  return $ alloc ++ fillClosure ++ [updateRTag, updateNode, adjustSpB, enter]

type StIntEnvBinds a = RS Env Int a

class SaveFree a where
  saveFree :: a -> StIntEnvBinds [CStatement]

instance SaveFree a => SaveFree (Maybe a) where
  saveFree (Just a) = saveFree a
  saveFree Nothing = return []

instance SaveFree STGAlts where
  saveFree (STGAAlts aalts maybeDalt) = saveFree' aalts maybeDalt
  saveFree (STGPAlts palts maybeDalt) = saveFree' palts maybeDalt

saveFree' alts maybeDalt = do
  statements <- liftM concat $ mapM saveFree alts
  saveDalt <- saveFree maybeDalt
  return $ statements ++ saveDalt

instance SaveFree STGAAlt where
  saveFree alt = saveAlt $ free alt

instance SaveFree STGPAlt where
  saveFree palt = saveAlt $ free palt

instance SaveFree STGDAlt where
  saveFree dalt = saveAlt $ free dalt

saveAlt :: RBinds Bindings -> StIntEnvBinds [CStatement]
saveAlt free = do
    i <- get
    env <- ask
    let fVars = Set.toList $ runReader free (env^.gEnv.globs)
    mapM saveVar fVars

saveVar :: Var -> StIntEnvBinds CStatement
saveVar var = do
  i <- get
  env <- ask
  lookupEnv var env
    (const $ return $ CComment $ var ++ " is on the stack")
    (\n -> do
      put (i-1)
      let assign = assignStackA i $ offsetNode n
      return $ CAnn (var ++ " is in the current closure") assign)
    (\n -> do
      put (i-1)
      let assign = assignStackA i $ offsetHeap n
      return $ CAnn (var ++ " is in the heap") assign)
    (return $ CComment $ var ++ " is a global")

-- Pretty printing

instance Pretty CProgram where
  pp (CProgram declStrings vars functions) = vcat $ punctuate line
    [vcat (map text declStrings), pp vars, pp functions]

instance Pretty CVarDecl where
  pp CVarDecl{..} = pp varType <+> text varName <> size' <>
    initializer' <> semi
    where size' = case varArrSize of Just 0 -> brackets empty
                                     Just n -> brackets $ int n
                                     Nothing -> empty
          initializer' = case varExpr of
                          Just expr -> space <> equals <+> pp expr
                          Nothing -> empty

{- Wrap a document in C-style braces -}
cBraces :: Doc -> Doc
cBraces doc = vcat [lbrace, indent 4 doc, rbrace]

instance Pretty CFunction where
  pp CFunction{..} = pp funType <+> text funName <> parens (pp funArgs)
    <+> cBraces (pp funBody)
  ppList = vcat . punctuate line . map pp

instance Pretty CType where
  pp CInt = text "int"
  pp (CTypeDef name) = text name
  pp (CPointerType ty) = pp ty <> char '*'

instance Pretty CParam where
  pp (CParam ty name) = pp ty <+> text name
  ppList = commas . map pp

instance Pretty CStatement where
  pp (CSVarDecl varDecl) = pp varDecl
  pp (CSExpr expr) = pp expr <> semi
  pp (CSIf cond th el) = text "if" <+> parens (pp cond) <+> cBraces (pp th)
    <+> elBlock
    where elBlock | null el = empty
                  | otherwise = text "else" <+> cBraces (vcat $ map pp el)
  pp (CSWhile cond body) = text "while" <+> parens (pp cond) <+>
    cBraces (pp body)
  pp (CSwitch e cases) = text "switch" <+> parens (pp e) <+> cBraces (pp cases)
  pp (CSReturn maybeExpr) = text "return" <+> pp maybeExpr <> semi
  pp (CSEnter name) = text "ENTER" <> parens (text name) <> semi
  pp (CJump name) = text "JUMP" <> parens (text name) <> semi
  pp (CComment comment) = enclose (text "/* ") (text " */") (pp comment)
  pp (CAnn comment statement) = fill 30 (pp statement) <+>
    enclose (text "/* ") (text " */") (pp comment)

instance Pretty CCase where
  pp (CCase (-1) body) = text "default" <> colon <$$>
    indent 4 (pp body <$$> text "break" <> semi)
  pp (CCase i body) = text "case" <+> int i <> colon <$$>
    indent 4 (pp body <$$> text "break" <> semi)

instance Pretty CExpr where
  pp (CID name) = text name
  pp (COp op e1 e2) = pp e1 <+> pp op <+> pp e2
  pp (CAssign name index expr) = text name <> bracks' <+> equals <+> pp expr
    where bracks' = case index of Just i -> brackets $ int i
                                  Nothing -> empty
  pp (CLit lit) = pp lit
  pp (CString string) = dquotes $ text string
  pp (CPointer name) = char '*' <> text name
  pp (CCall expr args) = pp expr <> parens (pp args)
  pp (CCast ty e) = parens (pp ty) <> pp e
  pp (CArray exprs) = encloseSep lbrace rbrace comma (map pp exprs)
  pp (CArrayElement arr i) = text arr <> brackets (int i)
  pp (CParens e) = parens $ pp e

instance Pretty COp where
  pp CEq = text "=="
  pp CNe = text "!="
  pp CLt = char '<'
  pp CPlus = char '+'
  pp CMinus = char '-'
