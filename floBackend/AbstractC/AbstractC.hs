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
import Data.List (partition)
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
      intRegDecl] ++ prototypes ++ finalDecls^.varDecls ++
      finalDecls^.infoTables ++ finalDecls^.closures)
    (finalDecls^.stdEntries ++ [cMain])
    where
    -- After the conversion, add qualified names to all let-bound
    -- variables so there are no name clashes when we lift the definitions
    STGProgram{..} = addPrefixes prog

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

    -- Set up initial environment and state for the compilation
    initialEnv = Env (LocalEnv [] [] [] []) (GlobalEnv globs dataConses)
    initialState = St (Decls [] [] [] []) 1
    globs = Set.fromList $ map (\(STGBinding name _) -> name) stgBindings
    -- Tag the data constructors with unique numbers
    dataConses = zipWith (\(STGDataCons cons _) i -> (cons,i))
                 stgDataConses [1..]

    -- Static closures, info tables, var declarations, and standard entry code
    finalState = execRS (mapM_ compileDC stgDataConses >>
                         mapM_ compileTopBind stgBindings)
                 initialEnv initialState
    finalDecls = finalState^.decls

    -- Function prototypes
    prototypes = map createFunPrototype (cMain : finalDecls^.stdEntries)

{- Compiles data constructors -}
compileDC :: STGDataCons -> SEnv ()
compileDC (STGDataCons cons _) = do
  -- Create the standard entry code and info table
  stdEntry <- createDCStdEntry cons
  infoTable <- createInfoTable cons stdEntry
  return ()

{- Creates the standard entry code for data constructors -}
createDCStdEntry :: Cons -> SEnv CFunction
createDCStdEntry cons = do
  env <- ask
  let -- Update RTag with the tag of the constructor
      updateRTag = assignRTag $ CLit $ lookupTag cons env
      stdEntry = CFunction pointerTD (cons ++ "_entry") [] $
                 updateRTag : popRetEnter
  -- Add the function to the state
  modify (decls.stdEntries %~ (stdEntry:))
  return stdEntry

{- Creates an info table that includes the given standard entry code -}
createInfoTable :: ID -> CFunction -> SEnv InfoTable
createInfoTable name CFunction{..} = do
  let infoTable = CVarDecl pointerTD (name ++ "_info")
                  (Just 0) (Just $ CArray [CCast pointerTD $ CID funName])
  -- Add the info table to the state
  modify (decls.infoTables %~ (infoTable:))
  return infoTable

{- Top-level bindings are compiled the same as nested bindings, with the
   addition of a static closure. -}
compileTopBind :: STGBinding -> SEnv ()
compileTopBind b@(STGBinding name _) = do
  CVarDecl{..} <- compileBind b
  -- Create the static closure
  let closure = CVarDecl pointerTD (name ++ "_closure")
                (Just 0) (Just $ CArray [CCast pointerTD $ CID varName])
  -- Add the closure to the state
  modify (decls.closures %~ (closure:))
  return ()

{- Compiling an STG binding involves compiling the body of the lambda form into
   standard entry code, and creating a static info table. -}
compileBind :: STGBinding -> SEnv InfoTable
compileBind (STGBinding name LambdaForm{..}) = do
      -- Partition the arguments into primitive and non-primitive. By
      -- convention, primitive values end with a '#'.
  let (bStackArgs, aStackArgs) = partition (('#' ==) . last) args
  -- Compile the expression in the modified environment.
  expr' <- local (lEnv .~ LocalEnv
    -- Non-primitive arguments are located at offsets from the A stack pointer.
    (zip aStackArgs [0..])
    -- Primitive arguments are located at offsets from the B stack pointer.
    (zip bStackArgs [0,-1..])
    -- The free variables are located at offsets from Node.
    (zip (Set.toList fVars) [1..])
    -- Dynamically allocated closures are only valid for a single instruction
    -- sequence, so they need to be reset.
    [])
    (compileE expr)
      -- Todo: Implement these
  let stackOverflowCheck = []
      heapOverflowCheck = []
      -- Standard entry code for the lambda form
      stdEntry = CFunction pointerTD (name ++ "_entry") [] $
                 stackOverflowCheck ++ heapOverflowCheck ++ expr'
  -- Add the function to the state
  modify (decls.stdEntries %~ (++ [stdEntry]))
  -- Create the info table
  createInfoTable name stdEntry

{- Compiles an arbitrary STG expression -}
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
compileAp name args = do
  env <- ask
  let
    -- Grab all of the arguments into local variables.
    grabArgs = map (grabArg True) (env^.lEnv.localAStack) ++
               map (grabArg False) (env^.lEnv.localBStack)

    -- Grabs an argument from either stack into a local variable
    grabArg :: Bool -> (Var,Int) -> CStatement
    grabArg isA (arg,i) =
      CAnn ("Grab " ++ arg ++ " into a local variable") $ CSVarDecl $
      CVarDecl pointerTD (toVarName i) Nothing (Just $ CArrayElement stack i)
      where stack = if isA then spA else spB
            toVarName = if isA then ('a':) . show else ('b':) . show . negate

    -- Push the arguments (in reverse order) to the new function call on the
    -- appropriate stacks
    (pushArgs, (a',b')) = runState (mapM pushArg (reverse args))
      (length $ env^.lEnv.localAStack, negate $ length $ env^.lEnv.localBStack)

    -- Push an argument onto the appropriate stack
    pushArg :: Atom -> State (Int,Int) CStatement
    pushArg arg
      -- Boxed arguments get pushed onto the A stack, which grows downwards
      | isBoxed arg = do
          (a,b) <- get
          let AtomVar var = arg
              a' = a - 1
          put (a',b)
          return $ annotation $ assignStackA a' $ lookupVarExpr var env
      -- Unboxed arguments get pushed onto the B stack, which grows upwards
      | otherwise = do
          (a,b) <- get
          let expr = case arg of AtomVar var -> lookupVarExpr var env
                                 AtomLit lit -> CCast pointerTD $ CLit lit
              b' = b + 1
          put (a,b')
          return $ annotation $ assignStackB b' expr
      where annotation = CAnn $ "Push " ++ showP arg ++ " onto stack"
            -- An atom is boxed iff it's a variable that doesn't end with '#'
            isBoxed (AtomVar var) = last var /= '#'
            isBoxed _ = False

    -- Adjust the stack pointers
    adjustSp = adjust spA a' ++ adjust spB b'
    adjust name diff
      | diff == 0 = []
      | otherwise = [CAnn ("Adjust " ++ name) $
                     CSExpr $ CAssign name Nothing $ offset name diff]

  return $ grabArgs ++ pushArgs ++ adjustSp ++ updateNodeEnter name env

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
  infoTable <- alterState snd (set _2) $ compileBind bind

  let assignInfoTable = assignHeap i
        (CCast pointerTD $ CID $ varName infoTable)

      fVarE fVar = lookupEnv fVar env
        (CArrayElement spA) undefined offsetNode (CCast pointerTD . offsetHeap)
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
        (CArrayElement spA) undefined offsetNode (CCast pointerTD . offsetHeap)
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
  let convertAtom atom = case atom of AtomVar var -> lookupVarExpr var env
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
    undefined
    (\n -> do
      put (i-1)
      let assign = assignStackA i $ offsetNode n
      return $ CAnn (var ++ " is in the current closure") assign)
    (\n -> do
      put (i-1)
      let assign = assignStackA i $ offsetHeap n
      return $ CAnn (var ++ " is in the heap") assign)
    (return $ CComment $ var ++ " is a global")
