{-# LANGUAGE MultiParamTypeClasses, RecordWildCards, TypeSynonymInstances,
    FlexibleInstances, FlexibleContexts #-}
module AbstractC.AbstractC where

import AbstractC.Base
import Convertible
import Pretty
import STG

import Control.Arrow (first, second)
import Control.Monad.Reader
import Control.Monad.State
import Data.Either (partitionEithers)
import Data.List (partition)
import Data.Maybe (catMaybes, fromMaybe)
import qualified Data.Set as Set
import Lens.Micro

cMain :: CFunction
cMain = CFunction (CTypeDef "int") "main" []
        [f_main, cont, pushRet, pushWorld, interpreter, ret]
  where f_main = CSVarDecl $ CVarDecl functionTD "f_main" Nothing
          (Just $ CCast functionTD (CID "main"))
        cont = CSVarDecl $ CVarDecl functionTD "cont" Nothing
          (Just $ CID "main_entry")
        pushRet = CAnn "Push return address" $ assignStackB 0 $ CID "f_main"
        pushWorld = CAnn "Push initial world" $ assignStackA 0 $ CLit 0
        interpreter =  CSWhile (COp CNe (CID "cont") (CID "f_main"))
          [CSExpr $ CAssign "cont" Nothing $
           CCast functionTD (CCall (CParens $ CPointer $ CID "cont") [])]
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
    heapLimit = 1000000
    heap = CVarDecl pointerTD "Heap" (Just heapLimit) Nothing
    hpDecl = CVarDecl (CPointerType pointerTD) hp Nothing
      (Just $ COp CPlus (CID "Heap") (CLit heapLimit))
    hLimitDecl = CVarDecl (CPointerType pointerTD) hLimit Nothing
      (Just $ CID "Heap")
    -- Registers
    nodeDecl = CVarDecl (CPointerType pointerTD) node Nothing Nothing
    rTagDecl = CVarDecl CIntP rTag Nothing Nothing
    intRegDecl = CVarDecl CIntP intReg Nothing Nothing

    -- Set up initial environment and state for the compilation
    initEnv = initialEnv globs dataConses
    globs = Set.fromList $ map (\(STGBinding name _) -> name) stgBindings
    -- Tag the data constructors with unique numbers
    dataConses = zipWith (\(STGDataCons cons _) i -> (cons,i))
                 stgDataConses [1..]

    -- Static closures, info tables, var declarations, and standard entry code
    finalState = execRS (mapM_ compileDC stgDataConses >>
                         mapM_ compileTopBind stgBindings) initEnv initialState
    finalDecls = finalState^.decls

    -- Function prototypes
    prototypes = map createFunPrototype (cMain : finalDecls^.stdEntries)

{- Compiles data constructors -}
compileDC :: STGDataCons -> RS Env St ()
compileDC (STGDataCons cons _) = do
  -- Create the standard entry code and info table
  stdEntry <- createDCStdEntry cons
  infoTable <- createInfoTable cons stdEntry
  return ()

{- Creates the standard entry code for data constructors -}
createDCStdEntry :: Cons -> RS Env St CFunction
createDCStdEntry cons = do
  -- Update RTag with the tag of the constructor
  updateRTag <- asks $ assignRTag . CLit . lookupTag cons
  -- Pop the return address and jump to it
  popRetJump' <- asks $ evalState popRetJump

  let stdEntry = CFunction pointerTD (cons ++ "_entry") [] $
                 printFunName cons : updateRTag : popRetJump'
  -- Add the function to the state
  modify (decls.stdEntries %~ (stdEntry:))
  return stdEntry

{- Creates an info table that includes the given standard entry code -}
createInfoTable :: ID -> CFunction -> RS Env St InfoTable
createInfoTable name CFunction{..} = do
  let infoTable = CVarDecl pointerTD (name ++ "_info")
                  (Just 0) (Just $ CArray [CCast pointerTD $ CID funName])
  -- Add the info table to the state
  modify (decls.infoTables %~ (infoTable:))
  return infoTable

{- Top-level bindings are compiled the same as nested bindings, with the
   addition of a static closure. -}
compileTopBind :: STGBinding -> RS Env St ()
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
compileBind :: STGBinding -> RS Env St InfoTable
compileBind (STGBinding name LambdaForm{..}) = do
      -- Partition the arguments into primitive and non-primitive. By
      -- convention, primitive values end with a '$'.
  let (bStackArgs, aStackArgs) = partition (('$' ==) . last) args

  -- Create the new local environment to compile the body in
  newLocalEnv <- asks (lEnv .~ LocalEnv
    -- Initially there are no local variables
    []
    -- Non-primitive arguments are located at offsets from the A stack pointer.
    (zip aStackArgs [0..])
    -- Primitive arguments are located at offsets from the B stack pointer.
    (zip bStackArgs [0,-1..])
    -- The free variables are located at offsets from Node.
    (zip (Set.toList fVars) [1..])
    -- Dynamically allocated closures are only valid for a single instruction
    -- sequence, so they need to be reset.
    [])

  -- Compile the expression in the modified environment.
  expr' <- local (const newLocalEnv) $ stateToRS $ compileE expr False

      -- Todo: Implement these
  let stackOverflowCheck = []
      heapOverflowCheck = []
      -- Standard entry code for the lambda form
      stdEntry = CFunction pointerTD (name ++ "_entry") [] $
                 printFunName name :
                 stackOverflowCheck ++ heapOverflowCheck ++ expr'
  -- Add the function to the state
  modify (decls.stdEntries %~ (++ [stdEntry]))
  -- Create the info table
  createInfoTable name stdEntry

{- Compiles an arbitrary STG expression -}
compileE :: STGExpr -> Bool -> State (Env,St) [CStatement]
compileE e saveEnv = case e of
  STGAp name args -> envToEnvSt $ compileAp name args saveEnv
  STGLet _ binds expr -> compileLet binds expr saveEnv
  STGCase (STGAp "ccall$" (proc:args))
          (STGAAlts [STGAAlt "MkIORes$" [n,w] alt] Nothing) ->
           compileCCall n w proc args alt saveEnv
  STGCase expr alts -> compileCase expr alts saveEnv
  STGCons cons args -> envToEnvSt $ compileCons cons args saveEnv
  STGLit lit -> envToEnvSt $ compileLit lit saveEnv
  STGPrim op e1 e2 -> envToEnvSt $ compilePrim op e1 e2 saveEnv

{- Compiles function applications -}
compileAp :: Var -> [Atom] -> Bool -> State Env [CStatement]
compileAp name args saveEnv = do
  env <- get
  let
    -- Grab all of the arguments into local variables.
    (localBinds,grabArgs) =
      unzip $ map (grabArg True) (env^.lEnv.localAStack) ++
              map (grabArg False) (env^.lEnv.localBStack)

    -- Grabs an argument from either stack into a local variable
    grabArg :: Bool -> (Var,Int) -> ((Var,Var),CStatement)
    grabArg isA (arg,i) = ((arg,toVarName i),
      CAnn ("Grab " ++ arg ++ " into a local variable") $ CSVarDecl $
      CVarDecl pointerTD (toVarName i) Nothing (Just $ CArrayElement stack i))
      where stack = if isA then spA else spB
            toVarName = if isA then ('a':) . show else ('b':) . show . negate

  -- Update the environment to include the new local variables
  modify $ lEnv.localVars %~ (++ localBinds)

  env <- get
  let
    -- Push the arguments (in reverse order) to the new function call on the
    -- appropriate stacks
    pushStarts | saveEnv = (0,0)
               | otherwise = (length $ env^.lEnv.localAStack,
                              negate $ length $ env^.lEnv.localBStack)
    (pushArgs, (a',b')) = runState (mapM pushArg (reverse args)) pushStarts

    -- Push an argument onto the appropriate stack
    pushArg :: Atom -> State (Int,Int) CStatement
    pushArg arg
      -- Boxed arguments get pushed onto the A stack, which grows downwards
      | isBoxedA arg = do
          (a,b) <- get
          let AtomVar var = arg
              a' = a - 1
          put (a',b)
          return $ annotation $ assignStackA a' $ lookupVarExpr var env
      -- Unboxed arguments get pushed onto the B stack, which grows upwards
      | otherwise = do
          (a,b) <- get
          let expr = case arg of AtomVar var -> lookupVarExpr var env
                                 AtomLit lit -> CLit lit
              b' = b + 1
          put (a,b')
          return $ annotation $ assignStackB b' expr
      where annotation = CAnn $ "Push " ++ showP arg ++ " onto stack"

  -- Adjust stack pointers
  adjustStacks <- adjustSps a' b'

  -- If the variable to call is boxed, load it into node and enter it. If it's
  -- primitive, load it into the appropriate register and jump to the return
  -- address on the B stack.
  enter <- if isBoxed name
            then gets $ updateNodeEnter name
            else updateIntRegRet True $ CCast CIntP $ lookupVarExpr name env

  return $ grabArgs ++ pushArgs ++ adjustStacks ++ enter

{- Compiles let(rec) expressions -}
compileLet :: [STGBinding] -> STGExpr -> Bool -> State (Env,St) [CStatement]
compileLet binds expr saveEnv = do
  (env,s) <- get
  let
    -- Allocate space in the heap for the new closures and update the
    -- environment with the new let bindings
    (alloc,env') = second (lEnv.dynamicFree %~ (++ newBinds)) $
      runState (allocateHeap spaceNeeded) env

    -- Fill in the closures
    (bindsAssigns, (spaceNeeded,s')) = runRS (mapM fillClosure binds) env' (0,s)
    (newBinds,fillClosures) = second concat $ unzip bindsAssigns

  -- Update the state
  put (env',s')

  -- Compile the body of the let expression in the new environment
  expr' <- compileE expr saveEnv
  return $ alloc ++ fillClosures ++ CComment "Evaluate body" : expr'

{- Fill in a heap-allocated closure -}
fillClosure :: STGBinding -> RS Env (Int,St) ((Var,Int),[CStatement])
fillClosure bind@(STGBinding name LambdaForm{..}) = do
  -- Get the current heap offset
  i <- gets fst
  env <- ask
  -- Compile the binding into an info table and standard entry code
  infoTable <- alterRS snd (set _2) $ compileBind bind

  let -- Assign the info table to the first spot in the closure
      assignInfoTable = assignHeap i $ CID $ varName infoTable

      -- Fill in the rest of the closure with the free variables
      assignFreeVars = zipWith (\i' fVar -> CAnn fVar $
        assignHeap i' (lookupVarExpr fVar env))
        [i+1..] (Set.toList fVars)

      assigns = assignInfoTable : assignFreeVars

  -- Update the heap offset
  modify $ first (+ length assigns)
  return ((name, i), CComment ("Fill in closure for " ++ name) : assigns)

{- Compiles case expressions -}
compileCase :: STGExpr -> STGAlts -> Bool -> State (Env,St) [CStatement]
compileCase e alts saveEnv = do
  -- Save the local environment
  saveLocalEnv' <- envToEnvSt saveLocEnv
  -- Compile the alternatives in the modified environment
  alts' <- rsToState $ compileAlts alts saveEnv

  -- Push the return address to the B stack
  offsetStackB' <- envToEnvSt $ offsetStackB 1
  let pushRet = [assignStackB 1 $ CID (funName alts'),
                 offsetStackB']

  -- Compile the expression in the (again) modified environment
  e' <- compileE e True

  return $ CComment "Save local environment" : saveLocalEnv' ++
           CComment "Push return address" : pushRet ++
           CComment "Evaluate body" : e'

{- Saves a local environment on the stack and returns the updated environment -}
saveLocEnv :: State Env [CStatement]
saveLocEnv = do
  env <- get
  let
    saveLocEnv' :: State (Int,Int) ([(Var,Int)],[(Var,Int)],[CStatement])
    saveLocEnv' = do
      (saveNodeStack, saveNodeStates) <- mapAndUnzipM
        (\(var,i) -> saveVar var (nodeOffset i))
        (env^.lEnv.localFree)
      (saveDynamicStack, saveDynamicStates) <- mapAndUnzipM
        (\(var,i) -> saveVar var (heapOffset i))
        (env^.lEnv.dynamicFree)
      let (saveA, saveB) = partitionEithers $ saveNodeStack ++ saveDynamicStack
      return (saveA, saveB, saveNodeStates ++ saveDynamicStates)

    -- Generate the new bindings and stack offsets
    ((saveA, saveB, saves), (a',b')) = runState saveLocEnv' (0,0)

  -- Update the environment to include the new stack items
  put (env & lEnv.localAStack %~ (++ saveA)
           & lEnv.localBStack %~ (++ saveB)
           & lEnv.localFree .~ []
           & lEnv.dynamicFree .~ [])

  -- Adjust the stack pointers
  adjustStacks <- adjustSps a' b'

  return $ saves ++ adjustStacks

{- Save a variable on the appropriate stack and return its new binding -}
saveVar :: Var -> CExpr -> State (Int,Int)
                           (Either (Var,Int) (Var,Int), CStatement)
saveVar var expr
  | isBoxed var = do
      (a,b) <- get
      let a' = a - 1
      put (a',b)
      return (Left (var,a'), ann $ assignStackA a' expr)
  | otherwise = do
      (a,b) <- get
      let b' = b + 1
      put (a,b')
      return (Right (var,b'), ann $ assignStackB b' expr)
  where ann = CAnn $ "Save " ++ var

{- Compile the alternatives of a case expression -}
compileAlts :: STGAlts -> Bool -> RS Env St CFunction
compileAlts alts saveEnv = do
  num <- gets (^.altsNum)
  -- Increase the number of alternatives
  modify (altsNum .~ num + 1)
  let
    altName = "alt" ++ show num

    -- Compiles algebraic alternatives
    makeCaseA (STGAAlt cons vars e) = do
      tag <- asks $ lookupTag cons
      -- Compile the alternative in a modified environment where the constructor
      -- arguments are bound to offsets from Node.
      let nodeBinds = zip vars [1..]
      e' <- local (lEnv.localFree .~ nodeBinds) $ stateToRS (compileE e saveEnv)
      return $ CCase tag e'

    -- Compiles primitive alternatives
    makeCaseP (STGPAlt lit e) =
      liftM (CCase lit) (stateToRS $ compileE e saveEnv)

    -- Compiles default alternatives
    makeCaseD (Just (STGDAlt (Just d) e)) = do
      -- Bind the default variable to the value returned (assume for now it's
      -- an int)
      let bindDef = CSVarDecl $ CVarDecl CIntP d Nothing (Just $ CID intReg)
      e' <- local (lEnv.localVars %~ ((d,d):)) $ stateToRS $ compileE e saveEnv
      return [CCase (-1) $ bindDef : e']
    makeCaseD (Just (STGDAlt Nothing e)) = do
      e' <- stateToRS $ compileE e saveEnv
      return [CCase (-1) e']
    makeCaseD Nothing = return []

  (tag,body) <- case alts of
    STGAAlts aalts maybeDalt -> do
      aalts' <- mapM makeCaseA aalts
      dalt' <- makeCaseD maybeDalt
      return (rTag, aalts' ++ dalt')
    STGPAlts palts maybeDalt -> do
      palts' <- mapM makeCaseP palts
      dalt' <- makeCaseD maybeDalt
      return (intReg, palts' ++ dalt')

  -- Create the function that performs case analysis. The jump to main at the
  -- end will hopefully never be executed but is provided to get rid of a
  -- compiler warning about control reaching the end of a non-void function.
  let fun = CFunction pointerTD altName []
            [printFunName altName, CSwitch (CID tag) body, CJump "main"]
  -- Append the alternatives to the list of functions
  modify $ decls.stdEntries %~ (++ [fun])
  return fun

{- Compiles data constructors -}
compileCons :: Cons -> [Atom] -> Bool -> State Env [CStatement]
compileCons cons args saveEnv = do
  -- Allocate space in the heap for the new closure
  alloc <- allocateHeap $ 1 + length args
  -- Update the environment to include it
  modify $ lEnv.dynamicFree %~ ((cons,0):)

  env <- get
  let comment = CComment $ "Fill in closure for " ++ cons
      -- Assign the info table
      assignInfoTable = assignHeap 0 $ CID $ cons ++ "_info"
      -- Assign the rest of the data constructor fields
      assigns = zipWith (\i arg -> case arg of
        AtomVar var -> CAnn var $ assignHeap i (lookupVarExpr var env)
        AtomLit lit -> CAnn (show lit) $ assignHeap i $ CLit lit)
        [1..] args

  -- Clear the local environment if necessary
  clearEnv' <- clearEnv saveEnv
  -- Update Node and enter
  updateNodeEnter' <- gets $ updateNodeEnter cons

  return $ alloc ++ comment:assignInfoTable:assigns ++ clearEnv' ++
           updateNodeEnter'

{- Compiles literals -}
compileLit :: Lit -> Bool -> State Env [CStatement]
compileLit lit saveEnv = updateIntRegRet saveEnv $ CLit lit

{- Compiles primitive operations -}
compilePrim :: Var -> Atom -> Atom -> Bool -> State Env [CStatement]
compilePrim op e1 e2 saveEnv = do
  env <- get
  let convertAtom atom = case atom of
        AtomVar var -> CCast CIntP $ lookupVarExpr var env
        AtomLit lit -> CLit lit
  updateIntRegRet saveEnv $ COp (lookupOp op) (convertAtom e1) (convertAtom e2)

{- Save an int in a register and jump to the return address on the B stack -}
updateIntRegRet :: Bool -> CExpr -> State Env [CStatement]
updateIntRegRet saveEnv e = do
      -- Save expression in register
  let updateIntReg = assignIntReg e
  -- Clear the local environment if necessary
  clearEnv' <- clearEnv saveEnv
  -- Pop off the return vector and jump to it
  popRetJump' <- popRetJump
  return $ updateIntReg : clearEnv' ++ popRetJump'

{- Compile primitive calls to C -}
compileCCall :: Var -> Var -> Atom -> [Atom] -> STGExpr -> Bool ->
                State (Env,St) [CStatement]
compileCCall n w (AtomVar proc) args alt saveEnv = do
  env <- gets fst
  -- Compile the call to C
  let args' = map (\arg -> case arg of
                            AtomVar var -> CCast CInt $ lookupVarExpr var env
                            AtomLit lit -> CLit lit
                            AtomString s -> CString s) (init args)
      ccall = CSVarDecl $ CVarDecl CIntP n Nothing $
              Just $ CCall (CID proc) args'
      newWorld = CSVarDecl $ CVarDecl CIntP w Nothing $ Just $ CLit 0
  -- Add n and w to the environment
  modify $ first $ lEnv.localVars %~ ([(n,n),(w,w)] ++)
  -- Compile the alternatives
  alt' <- compileE alt saveEnv
  return $ ccall : newWorld : alt'
