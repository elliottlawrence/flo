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
import Data.Maybe (catMaybes)
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
    ([stack, spBDecl, spADecl, heap, hpDecl, hLimitDecl, nodeDecl, rTagDecl] ++
      prototypes ++ infoTables ++ closures ++ extras^.extraVarDecls)
    (stdEntries ++ extras^.extraFuns ++ [cMain])
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
    prototypes = map createFunPrototype (cMain:stdEntries ++ extras^.extraFuns)

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
