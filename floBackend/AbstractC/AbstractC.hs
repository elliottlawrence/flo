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
import Data.Either (partitionEithers)
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
    initialEnv = Env (LocalEnv [] [] [] [] []) (GlobalEnv globs dataConses)
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
  -- Update RTag with the tag of the constructor
  updateRTag <- asks $ assignRTag . CLit . lookupTag cons
  -- Pop the return address and enter it
  popRetEnter' <- asks $ evalState popRetEnter

  let stdEntry = CFunction pointerTD (cons ++ "_entry") [] $
                 updateRTag : popRetEnter'
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
      -- convention, primitive values end with a '$'.
  let (bStackArgs, aStackArgs) = partition (('$' ==) . last) args
  -- Compile the expression in the modified environment.
  expr' <- local (lEnv .~ LocalEnv
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
    (compileE expr False)
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
compileE :: STGExpr -> Bool -> SEnv [CStatement]
compileE e saveEnv = case e of
  STGAp name args -> liftREnv $ compileAp name args saveEnv
  STGLet _ binds expr -> compileLet binds expr saveEnv
  STGCase expr alts -> compileCase expr alts saveEnv
  STGCons cons args -> liftREnv $ compileCons cons args saveEnv
  STGLit lit -> liftREnv $ compileLit lit saveEnv
  STGPrim op e1 e2 -> liftREnv $ compilePrim op e1 e2 saveEnv

{- Compiles function applications -}
compileAp :: Var -> [Atom] -> Bool -> REnv [CStatement]
compileAp name args saveEnv = do
  env <- ask
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
    env' = env & (lEnv.localVars %~ (++ localBinds))

    -- Push the arguments (in reverse order) to the new function call on the
    -- appropriate stacks
    pushStarts | saveEnv = (0,0)
               | otherwise = (length $ env'^.lEnv.localAStack,
                              negate $ length $ env'^.lEnv.localBStack)
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
          return $ annotation $ assignStackA a' $ lookupVarExpr var env'
      -- Unboxed arguments get pushed onto the B stack, which grows upwards
      | otherwise = do
          (a,b) <- get
          let expr = case arg of AtomVar var -> lookupVarExpr var env'
                                 AtomLit lit -> CCast pointerTD $ CLit lit
              b' = b + 1
          put (a,b')
          return $ annotation $ assignStackB b' expr
      where annotation = CAnn $ "Push " ++ showP arg ++ " onto stack"

    -- Adjust stack pointers
    adjustStacks = evalState (adjustSps a' b') env'

    -- If the variable to call is boxed, load it into node and enter it. If it's
    -- primitive, load it into the appropriate register and jump to the return
    -- address on the B stack.
    enter | isBoxed name = updateNodeEnter name env'
          | otherwise = assignIntReg (CID name) : evalState popRetEnter env'

  return $ grabArgs ++ pushArgs ++ adjustStacks ++ enter

{- Compiles let(rec) expressions -}
compileLet :: [STGBinding] -> STGExpr -> Bool -> SEnv [CStatement]
compileLet binds expr saveEnv = do
  env <- ask
  s <- get
  let
    -- Allocate space in the heap for the new closures and update the
    -- environment with the new let bindings
    (alloc,env') = second (& lEnv.dynamicFree %~ (++ newBinds)) $
      runState (allocateHeap spaceNeeded) env

    -- Fill in the closures
    (bindsAssigns, (spaceNeeded,s')) = runRS (mapM fillClosure binds) env' (0,s)
    (newBinds,fillClosures) = second concat $ unzip bindsAssigns

  put s'
  -- Compile the body of the let expression in the new environment
  expr' <- local (const env') (compileE expr saveEnv)
  return $ alloc ++ fillClosures ++ CComment "Evaluate body" : expr'

{- Fill in a heap-allocated closure -}
fillClosure :: STGBinding -> RS Env (Int,St) ((Var,Int),[CStatement])
fillClosure bind@(STGBinding name LambdaForm{..}) = do
  -- Get the current heap offset
  i <- gets fst
  env <- ask
  -- Compile the binding into an info table and standard entry code
  infoTable <- alterState snd (set _2) $ compileBind bind

  let -- Assign the info table to the first spot in the closure
      assignInfoTable = assignHeap i (CCast pointerTD $ CID $ varName infoTable)

      -- Fill in the rest of the closure with the free variables
      assignFreeVars = zipWith (\i' fVar -> CAnn fVar $
        assignHeap i' (lookupVarExpr fVar env))
        [i+1..] (Set.toList fVars)

      assigns = assignInfoTable : assignFreeVars

  -- Update the heap offset
  modify $ first (+ length assigns)
  return ((name, i), CComment ("Fill in closure for " ++ name) : assigns)

{- Compiles case expressions -}
compileCase :: STGExpr -> STGAlts -> Bool -> SEnv [CStatement]
compileCase e alts saveEnv = do
  -- Save the local environment
  (saveLocalEnv,env) <- asks $ runState saveLocEnv

  -- Compile the alternatives in the modified environment
  alts' <- local (const env) $ compileAlts alts saveEnv

  -- Push the return address to the B stack
  let (offsetStackB',env') = runState (offsetStackB 1) env
      pushRet = [assignStackB 1 $ CCast pointerTD $ CID (funName alts'),
                 offsetStackB']

  -- Compile the expression in the (again) modified environment
  e' <- local (const env') $ compileE e True

  return $ CComment "Save local environment" : saveLocalEnv ++
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
        (\(var,i) -> saveVar var (CCast pointerTD $ heapOffset i))
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
compileAlts :: STGAlts -> Bool -> SEnv CFunction
compileAlts alts saveEnv = do
  num <- gets (^.altsNum)
  -- Increase the number of alternatives
  modify (& altsNum .~ num + 1)

  let
    altName = "alt" ++ show num

    -- Compiles algebraic alternatives
    makeCaseA (STGAAlt cons vars e) = do
      tag <- asks $ lookupTag cons
      -- Compile the alternative in a modified environment where the constructor
      -- arguments are bound to offsets from Node.
      let nodeBinds = zip vars [1..]
      e' <- local (& lEnv.localFree .~ nodeBinds) $ compileE e saveEnv
      return $ CCase tag e'

    -- Compiles primitive alternatives
    makeCaseP (STGPAlt lit e) = liftM (CCase lit) (compileE e saveEnv)

    -- Compiles default alternatives
    makeCaseD (Just (STGDAlt (Just d) e)) = do
      -- Bind the default variable to the value returned (assume for now it's
      -- an int)
      let bindDef = CSVarDecl $ CVarDecl CInt d Nothing (Just $ CID intReg)
      e' <- local (lEnv.localVars %~ ((d,d):)) $ compileE e saveEnv
      return [CCase (-1) $ bindDef : e']
    makeCaseD (Just (STGDAlt Nothing e)) = do
      e' <- compileE e saveEnv
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
            [CSwitch (CID tag) body, CJump "main"]
  -- Append the alternatives to the list of functions
  modify (& decls.stdEntries %~ (++ [fun]))
  return fun

{- Compiles data constructors -}
compileCons :: Cons -> [Atom] -> Bool -> REnv [CStatement]
compileCons cons args saveEnv = do
  -- Allocate space in the heap for the new closure and update the environment
  -- to include it
  (alloc,env) <- asks $ second (& lEnv.dynamicFree %~ ((cons,0):)) .
                        runState (allocateHeap $ 1 + length args)

  let comment = CComment $ "Fill in closure for " ++ cons
      -- Assign the info table
      assignInfoTable = assignHeap 0 (CCast pointerTD $ CID $ cons ++ "_info")
      -- Assign the rest of the data constructor fields
      assigns = zipWith (\i arg -> case arg of
        AtomVar var -> CAnn var $ assignHeap i (lookupVarExpr var env)
        AtomLit lit -> CAnn (show lit) $
                       assignHeap i (CCast pointerTD $ CLit lit))
        [1..] args

      -- Clear the local environment if necessary
      clearEnv' = evalState (clearEnv saveEnv) env

  return $ alloc ++ comment:assignInfoTable:assigns ++ clearEnv' ++
           updateNodeEnter cons env

{- Compiles literals -}
compileLit :: Lit -> Bool -> REnv [CStatement]
compileLit lit saveEnv = do
  env <- ask
      -- Save lit in register
  let updateIntReg = assignIntReg (CLit lit)
      -- Clear the local environment if necessary
      clearEnv' = evalState (clearEnv saveEnv) env
  -- Pop off the return vector and enter it
  popRetEnter' <- asks $ evalState popRetEnter
  return $ updateIntReg : clearEnv' ++ popRetEnter'

{- Compiles primitive operations -}
compilePrim :: Var -> Atom -> Atom -> Bool -> REnv [CStatement]
compilePrim op e1 e2 saveEnv = do
  env <- ask
  let convertAtom atom = case atom of
        AtomVar var -> CCast CInt $ lookupVarExpr var env
        AtomLit lit -> CLit lit
      -- Save lit in register
      updateIntReg = assignIntReg $
        COp (lookupOp op) (convertAtom e1) (convertAtom e2)
      -- Clear the local environment if necessary
      clearEnv' = evalState (clearEnv saveEnv) env
  -- Pop off the return vector and enter it
  popRetEnter' <- asks $ evalState popRetEnter
  return $ updateIntReg : clearEnv' ++ popRetEnter'
