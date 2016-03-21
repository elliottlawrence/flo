{-# LANGUAGE MultiParamTypeClasses, RecordWildCards, TypeSynonymInstances,
    FlexibleInstances #-}
module AbstractC where

import Convertible
import Pretty
import STG

import Control.Arrow (second, (***))
import Control.Monad.Reader
import Control.Monad.State
import Data.Maybe (catMaybes)
import qualified Data.Set as Set
import Text.PrettyPrint.Leijen hiding (Pretty)

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
                | CSReturn (Maybe CExpr)
                | CSEnter ID
                | CJump ID
                | CComment String                     -- Comments
                | CAnn String CStatement              -- Statement with comment
                | CNested (Either CFunction CVarDecl)

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

pointerTD :: CType
pointerTD = CTypeDef "pointer"

functionTD :: CType
functionTD = CTypeDef "function"

spA, spB, hp, hLimit, node :: String
spA = "SpA"
spB = "SpB"
hp = "Hp"
hLimit = "HLimit"
node = "Node"

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
  convert STGProgram{..} = CProgram
    [stdio, stdlib, enterMacro, jumpMacro, pointerTypeDef, functionTypeDef]
    ([stack, spBDecl, spADecl, heap, hpDecl, hLimitDecl, nodeDecl] ++ prototypes
      ++ infoTables ++ closures ++ varDecls)
    (stdEntries ++ [cMain])
    where -- Random declarations
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

          -- Static closures
          (closures, infoTables, stdEntries') = unzip3 $ map convert stgBindings
          (varDecls, stdEntries) = concat *** concat $
            unzip $ map flattenFun stdEntries'
          prototypes = map createFunPrototype (cMain:stdEntries)

createFunPrototype :: CFunction -> CVarDecl
createFunPrototype CFunction{..} = CVarDecl funType (funName ++ "()")
  Nothing Nothing

{- Flattens a function with nested declarations -}
flattenFun :: CFunction -> ([CVarDecl],[CFunction])
flattenFun f@CFunction{..} = (varDecls,funs ++ [f {funBody = funBody'}])
  where (maybeFunBody,varDecls',funs') = unzip3 $ map extractNested funBody
        varDecls = concat varDecls'
        funs = concat funs'
        funBody' = catMaybes maybeFunBody

{- Extracts nested declarations from a statement -}
extractNested :: CStatement -> (Maybe CStatement,[CVarDecl],[CFunction])
extractNested (CNested (Left fun)) = (Nothing,varDecls,funs)
  where (varDecls,funs) = flattenFun fun
extractNested (CNested (Right varDecl)) = (Nothing,[varDecl],[])
extractNested s = (Just s,[],[])

type Closure = CVarDecl
type InfoTable = CVarDecl
type StdEntry = CFunction

{- Converts top-level bindings with static closures -}
instance Convertible STGBinding (Closure, InfoTable, StdEntry) where
  convert b@(STGBinding name lForm) = (closure, infoTable, stdEntry)
    where closure = createClosure name infoTable
          (infoTable, stdEntry) = convert b

{- Converts local bindings which have dynamically allocated closures -}
instance Convertible STGBinding (InfoTable, StdEntry) where
  convert (STGBinding name lForm) = (infoTable, stdEntry)
    where infoTable = createInfoTable name stdEntry
          stdEntry = convert (name, lForm)

createClosure :: ID -> InfoTable -> CVarDecl
createClosure name CVarDecl{..} = CVarDecl pointerTD (name ++ "_closure")
  (Just 0) (Just $ CArray [CCast pointerTD $ CID varName])

createInfoTable :: ID -> CFunction -> CVarDecl
createInfoTable name CFunction{..} = CVarDecl pointerTD (name ++ "_info")
  (Just 0) (Just $ CArray [CCast pointerTD $ CID funName])

{- The local environment consists of locations on the stack, locations in the
   current closure, and dynamically allocated closures. -}
data LocalEnv = LocalEnv {
  localStack :: [(Var,Int)],
  localFree :: [(Var,Int)],
  dynamicFree :: [(Var,Int)]
}

{- Lookup a variable. If it's in the local environment, apply the corresponding
   function to its index on the stack or heap. If not, return the default
   value. -}
lookupEnv :: Var -> LocalEnv -> (Int -> a) -> (Int -> a) -> (Int-> a) -> a -> a
lookupEnv name (LocalEnv stack heap letBinds) justS justH justL nothing =
  case lookup name stack of
    Just i -> justS i
    Nothing -> case lookup name heap of
                 Just i -> justH i
                 Nothing -> case lookup  name letBinds of
                              Just i -> justL i
                              Nothing -> nothing

{- Adjusts all the heap offsets for dynamically allocated closures. Useful when
   the heap pointer has changed but we still need to reference these closures.
   -}
adjustDynamicFree :: Int -> [(Var,Int)] -> [(Var,Int)]
adjustDynamicFree i free = [ (var,index + i) | (var,index) <- free]

instance Convertible (Var, LambdaForm) CFunction where
  convert (name, LambdaForm{..}) = CFunction pointerTD (name ++ "_entry") [] $
    argSatCheck ++ stackOverflowCheck ++ heapOverflowCheck ++ makeUpdateFrame ++
    convert (expr,env)
    where env = LocalEnv (zip args [0..])
                         (zip (Set.toList fVars) [1..])
                         [] -- References to dynamically-allocated closures are
                            -- only valid within a single instruction sequence
          -- Todo: Implement these
          argSatCheck = []
          stackOverflowCheck = []
          heapOverflowCheck = []
          makeUpdateFrame = []

instance Convertible (STGExpr, LocalEnv) [CStatement] where
  convert (e,env) = case e of
    STGAp name args -> compileAp env name args
    STGLet _ binds expr -> compileLet env binds expr
    _ -> [CJump "main"]

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

{- Compiles function applications -}
compileAp :: LocalEnv -> Var -> [Atom] -> [CStatement]
compileAp env@(LocalEnv argsMap _ _) name as = grabArgs ++ pushArgs ++ adjustSp
  ++ [updateNode] ++ [enter]
  where
  -- Update node with the address of the function to call
  updateNode = CAnn ("Grab " ++ name ++ " into Node") $
    assignNode $ lookupVarExpr name
  enter = CAnn ("Enter " ++ name) $ CSEnter $ "(pointer**)" ++ node

  -- Lookup a name in the environment and convert it to an expression
  lookupVarExpr name = lookupEnv name env
    (CID . ("t" ++) . show) offsetNode (CCast pointerTD . offsetHeap)
    (CCast pointerTD $ CID $ name ++ "_closure")

  -- Grab all of the arguments into local variables.
  grabArgs = map grabArg argsMap

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
    assignStackA (length argsMap - n) $ lookupVarExpr var

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
  diffA = length argsMap - numA
  diffB = numB

{- Compiles let(rec) expressions -}
compileLet :: LocalEnv -> [STGBinding] -> STGExpr -> [CStatement]
compileLet env binds expr = allocateHeap ++ fillClosures ++
  callExpr
  where
  -- Allocate enough space in the heap for the closures
  allocateHeap = [
    CAnn "Allocate some heap" $
    CSExpr $ CAssign hp Nothing $ offsetHeap (negate spaceNeeded),
    CSIf (COp CLt (CID hp) (CID hLimit))
      [CSExpr $ CCall (CID "printf") [CString "Error: Out of heap space\\n"],
       CSExpr $ CCall (CID "exit") [CLit 0]]
      [] ]

  -- The updated environment, with the new bindings from the let expression
  env' = env {dynamicFree = adjustDynamicFree spaceNeeded (dynamicFree env)
    ++ newBinds}

  -- Fill in the closures, allowing for recursive definitions
  (bindsAssigns, spaceNeeded) =
    runReader (runStateT (mapM fillClosure binds) 0) newBinds
  (newBinds,fillClosures) = second concat $ unzip bindsAssigns

  -- Call the expression in the body of the let
  callExpr = CComment "Evaluate body" : convert (expr,env')

  -- Fill in a heap-allocated closure
  fillClosure :: STGBinding ->
    StateT Int (Reader [(Var,Int)]) ((Var,Int),[CStatement])
  fillClosure bind@(STGBinding name LambdaForm{..}) = do
    -- Get the current heap offset
    i <- get

    let (infoTable,stdEntry) = convert bind
        assignInfoTable = assignHeap i
          (CCast pointerTD $ CID $ varName infoTable)

        fVarE fVar = lookupEnv fVar env'
          (CArrayElement spA) offsetNode (CCast pointerTD . offsetHeap)
          (CCast pointerTD $ CID $ name ++ "_closure")
        assigns = assignInfoTable :
          zipWith (\i' fVar -> CAnn fVar $ assignHeap i' (fVarE fVar))
          [i+1..] (Set.toList fVars)

        comment = CComment $ "Fill in closure for " ++ name

    -- Update the heap offset
    put $ i + length assigns

    return ((name, i),
      CNested (Left stdEntry):CNested (Right infoTable):comment:assigns)

-- Pretty printing functions
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
  pp (CSReturn maybeExpr) = text "return" <+> pp maybeExpr <> semi
  pp (CSEnter name) = text "ENTER" <> parens (text name) <> semi
  pp (CJump name) = text "JUMP" <> parens (text name) <> semi
  pp (CComment comment) = enclose (text "/* ") (text " */") (pp comment)
  pp (CAnn comment statement) = fill 30 (pp statement) <+>
    enclose (text "/* ") (text " */") (pp comment)
  pp (CNested nest) = either pp pp nest

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
