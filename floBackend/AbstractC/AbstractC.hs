{-# LANGUAGE MultiParamTypeClasses, RecordWildCards, TypeSynonymInstances,
    FlexibleInstances, FlexibleContexts #-}
module AbstractC.AbstractC where

import AbstractC.Base
import Convertible
import Pretty
import STG

import Control.Arrow (first, second, (***))
import Control.Monad.Reader
import Control.Monad.State
import Data.Maybe (catMaybes, fromMaybe)
import qualified Data.Set as Set
import Lens.Micro

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
    ([stack, spBDecl, spADecl, heap, hpDecl, hLimitDecl, nodeDecl, rTagDecl,
      intRegDecl] ++ prototypes ++ finalState^.decls.varDecls ++
      finalState^.decls^.infoTables ++ finalState^.decls^.closures)
    (finalState^.decls.stdEntries ++ [cMain])
    where
    -- After the conversion, add qualified names to all let-bound
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
    spBDecl = CVarDecl (CPointerType pointerTD) spB Nothing (Just $ CID "Stack")
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
    intRegDecl = CVarDecl CInt intReg Nothing Nothing

    -- Static closures
    -- Tag the data constructors with unique numbers
    dataConses = zipWith (\(STGDataCons cons _) i -> (cons,i))
                 stgDataConses [1..]
    initialEnv = Env (LocalEnv [] [] [] []) (GlobalEnv globs dataConses)
    initialState = St (Decls [] [] [] []) 1

    finalState = execRS (mapM_ convertDC stgDataConses >>
                         mapM convertTopBind stgBindings)
                 initialEnv initialState
    prototypes = map createFunPrototype (cMain : finalState^.decls.stdEntries)

{- Converts data constructors -}
convertDC :: STGDataCons -> SEnv ()
convertDC (STGDataCons name _) = do
  -- Create the standard entry code and info table
  stdEntry <- createDCStdEntry name
  infoTable <- createInfoTable name stdEntry
  return ()

{- Creates the standard entry code for data constructors -}
createDCStdEntry :: Cons -> SEnv CFunction
createDCStdEntry cons = do
  env <- ask
  let
    -- Update RTag with the tag of the constructor
    updateRTag = assignRTag $ CLit $ lookupTag cons env
    -- Pop off the return vector and enter it
    adjustSpB = CSExpr $ CAssign spB Nothing $ offsetStackB (-1)
    enter = CAnn "Enter return address" $
            CSEnter $ "(pointer**)" ++ spB ++ "[1]"
    stdEntry = CFunction pointerTD (cons ++ "_entry") []
               [updateRTag, adjustSpB, enter]
  -- Add the function to the state
  modify (decls.stdEntries %~ (stdEntry:))
  return stdEntry

{- Converts top-level bindings with static closures -}
convertTopBind :: STGBinding -> SEnv ()
convertTopBind b@(STGBinding name lForm) = do
  infoTable <- convertLocBind b
  createClosure name infoTable
  return ()

{- Converts local bindings which have dynamically allocated closures -}
convertLocBind :: STGBinding -> SEnv InfoTable
convertLocBind (STGBinding name lForm) = do
  stdEntry <- convertLForm name lForm
  createInfoTable name stdEntry

{- Creates a static closure for a top-level binding -}
createClosure :: ID -> InfoTable -> SEnv ()
createClosure name CVarDecl{..} = do
  let closure = CVarDecl pointerTD (name ++ "_closure")
                (Just 0) (Just $ CArray [CCast pointerTD $ CID varName])
  -- Add the closure to the state
  modify (decls.closures %~ (closure:))

{- Creates an info table that includes the given standard entry code -}
createInfoTable :: ID -> CFunction -> SEnv InfoTable
createInfoTable name CFunction{..} = do
  let infoTable = CVarDecl pointerTD (name ++ "_info")
                  (Just 0) (Just $ CArray [CCast pointerTD $ CID funName])
  -- Add the info table to the state
  modify (decls.infoTables %~ (infoTable:))
  return infoTable

convertLForm :: Var -> LambdaForm -> SEnv CFunction
convertLForm name LambdaForm{..} = do
  -- References to dynamically-allocated closures are only valid within
  -- a single instruction sequence
  expr' <- local
    (lEnv .~ LocalEnv (zip args [0..]) [] (zip (Set.toList fVars) [1..]) [])
    (compileE expr)
      -- Todo: Implement these
  let argSatCheck = []
      stackOverflowCheck = []
      heapOverflowCheck = []
      makeUpdateFrame = []
      fun = CFunction pointerTD (name ++ "_entry") [] $
            argSatCheck ++ stackOverflowCheck ++ heapOverflowCheck ++
            makeUpdateFrame ++ expr'
  -- Add the function to the state
  modify (decls.stdEntries %~ (++ [fun]))
  return fun

compileE :: STGExpr -> SEnv [CStatement]
compileE e = case e of
  STGAp name args -> liftREnv $ compileAp name args
  STGLet _ binds expr -> compileLet binds expr
  STGCase expr alts -> compileCase expr alts
  STGCons cons args -> liftREnv $ compileCons cons args
  STGLit lit -> liftREnv $ compileLit lit
  STGPrim op e1 e2 -> liftREnv $ compilePrim op e1 e2

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
    grabArgs = map grabArg (env^.lEnv.localAStack)

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
      assignStackA (length (env^.lEnv.localAStack) - n) $ lookupVarExpr var

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
    diffA = length (env^.lEnv.localAStack) - numA
    diffB = numB

  return $ grabArgs ++ pushArgs ++ adjustSp ++ [updateNode] ++ [enter]

{- Compiles let(rec) expressions -}
compileLet :: [STGBinding] -> STGExpr -> SEnv [CStatement]
compileLet binds expr = do
  env <- ask
  s <- get
  let
    -- Fill in the closures, allowing for recursive definitions
    (bindsAssigns, (spaceNeeded,s')) = runRS (mapM fillClosure binds) env' (0,s)
    (newBinds,fillClosures) = second concat $ unzip bindsAssigns

    -- The updated environment, with the new bindings from the let expression
    env' = env & lEnv.dynamicFree .~
      adjustDynamicFree spaceNeeded (env^.lEnv.dynamicFree) ++ newBinds

  put s'
  expr' <- local (const env') (compileE expr)
      -- Call the expression in the body of the let
  let callExpr = CComment "Evaluate body" : expr'

  return $ allocateHeap spaceNeeded ++ fillClosures ++ callExpr

{- Fill in a heap-allocated closure -}
fillClosure :: STGBinding -> RS Env (Int,St) ((Var,Int),[CStatement])
fillClosure bind@(STGBinding name LambdaForm{..}) = do
  -- Get the current heap offset
  i <- gets fst
  env <- ask
  infoTable <- alterState snd (set _2) $ convertLocBind bind

  let assignInfoTable = assignHeap i
        (CCast pointerTD $ CID $ varName infoTable)

      fVarE fVar = lookupEnv fVar env
        (CArrayElement spA) offsetNode (CCast pointerTD . offsetHeap)
        (CCast pointerTD $ CID $ fVar ++ "_closure")
      assigns = assignInfoTable :
        zipWith (\i' fVar -> CAnn fVar $ assignHeap i' (fVarE fVar))
        [i+1..] (Set.toList fVars)

      comment = CComment $ "Fill in closure for " ++ name

  -- Update the heap offset
  modify $ first (+ length assigns)
  return ((name, i), comment:assigns)

{- Compiles case expressions -}
compileCase :: STGExpr -> STGAlts -> SEnv [CStatement]
compileCase e alts = do
  num <- gets (^.altsNum)
  -- Increase the number of alternatives
  modify (& altsNum .~ num + 1)

  -- Compile the expression and alternatives
  e' <- compileE e
  alts' <- compileAlts alts

  saveLocalEnv <- alterState (const 0) (const id) $ saveFree alts
  let altName = "alt" ++ show num
      pushRet = [assignStackB 1 $ CCast pointerTD $ CID altName,
                 CSExpr $ CAssign spB Nothing $ offsetStackB 1]
      callE = CComment "Evaluate body" : e'
      switchAlts = CFunction pointerTD altName [] [alts']

  -- Append the alternatives to the list of functions
  modify (& decls.stdEntries %~ (++ [switchAlts]))
  return $ saveLocalEnv ++ pushRet ++ callE

compileAlts :: STGAlts -> SEnv CStatement
compileAlts alts = do
  env <- ask
  let
    -- We can only compile default alternatives that don't have a bound
    -- variable, unless they're primitive.
    maybeCase (Just (STGDAlt _ e)) = do
      e' <- compileE e
      return [CCase (-1) e']
    maybeCase Nothing = return []

    makeCaseA (STGAAlt cons vars e) =
      liftM (CCase (lookupTag cons env)) (compileE e)
    makeCaseP (STGPAlt lit e) = liftM (CCase lit) (compileE e)

  case alts of
    STGAAlts aalts maybeDalt -> do
      aalts' <- mapM makeCaseA aalts
      dalt' <- maybeCase maybeDalt
      return $ CSwitch (CID rTag) $ aalts' ++ dalt'
    STGPAlts palts maybeDalt -> do
      palts' <- mapM makeCaseP palts
      dalt' <- maybeCase maybeDalt
      return $ CSwitch (CID intReg) $ palts' ++ dalt'

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

      -- Make node point to the current closure and enter it
      updateNode = CAnn ("Grab " ++ cons ++ " closure into Node") $
        assignNode $ CCast pointerTD $ CID hp
      enter = CAnn ("Enter " ++ cons) $ CSEnter $ "(pointer**)" ++ node

  return $ alloc ++ fillClosure ++ [updateNode, enter]

{- Compiles literals -}
compileLit :: Lit -> REnv [CStatement]
compileLit lit = do
  let -- Save lit in register
      updateIntReg = assignIntReg (CLit lit)
      -- Pop off the return vector and enter it
      adjustSpB = CSExpr $ CAssign spB Nothing $ offsetStackB (-1)
      enter = CAnn "Enter return address" $
              CSEnter $ "(pointer**)" ++ spB ++ "[1]"
  return [adjustSpB, enter]

{- Compiles primitive operations -}
compilePrim :: Var -> Atom -> Atom -> REnv [CStatement]
compilePrim op e1 e2 = do
  env <- ask
  let -- Lookup a name in the environment and convert it to an expression
      lookupVarExpr name = lookupEnv name env
        (CID . ("t" ++) . show) offsetNode offsetHeap
        (CID $ name ++ "_closure")

      convertAtom atom = case atom of AtomVar var -> lookupVarExpr var
                                      AtomLit lit -> CLit lit

      result = COp (lookupOp op) (convertAtom e1) (convertAtom e2)
      -- Save lit in register
      updateIntReg = assignIntReg result
      -- Pop off the return vector and enter it
      adjustSpB = CSExpr $ CAssign spB Nothing $ offsetStackB (-1)
      enter = CAnn "Enter return address" $
              CSEnter $ "(pointer**)" ++ spB ++ "[1]"
  return [updateIntReg, adjustSpB, enter]

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
