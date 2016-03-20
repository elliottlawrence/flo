{-# LANGUAGE MultiParamTypeClasses, RecordWildCards, TypeSynonymInstances,
    FlexibleInstances #-}
module AbstractC where

import Convertible
import Pretty
import STG

import Control.Arrow (first)
import Control.Monad.State
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
           | CPointer ID
           | CCall CExpr [CExpr]
           | CCast CType CExpr
           | CArray [CExpr]
           | CArrayElement ID Int

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
           CCast functionTD (CCall (CPointer "cont") [])]
        ret = CSReturn $ Just $ CLit 0

instance Convertible STGProgram CProgram where
  convert STGProgram{..} = CProgram
    [stdio, enterMacro, jumpMacro, pointerTypeDef, functionTypeDef]
    ([stack, spBDecl, spADecl, heap, hpDecl, hLimitDecl, nodeDecl] ++ prototypes
      ++ infoTables ++ closures)
    (stdEntries ++ [cMain])
    where -- Random declarations
          stdio = "#include <stdio.h>"
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
          (closures, infoTables, stdEntries) = unzip3 $ map convert stgBindings
          prototypes = map createFunPrototype (cMain:stdEntries)

createFunPrototype :: CFunction -> CVarDecl
createFunPrototype CFunction{..} = CVarDecl funType (funName ++ "()")
  Nothing Nothing

type Closure = CVarDecl
type InfoTable = CVarDecl
type StdEntry = CFunction

{- Converts top-level bindings with status closures -}
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

instance Convertible (Var, LambdaForm) CFunction where
  convert (name, LambdaForm{..}) = CFunction pointerTD (name ++ "_entry") [] $
    case expr of STGAp name as -> compileAp fVars' args name as
                 STGLet rec binds expr -> compileLet fVars' args binds expr
                 _ -> [CJump "main"]
    where fVars' = zip (Set.toList fVars) [1..]

{- Lookup a variable. If it's in the local environment, apply the corresponding
   function to its index on the stack or heap. If not, return the default
   value. -}
lookupEnv :: Var -> [(Var,Int)] -> [(Var,Int)] ->
            (Int -> a) -> (Int -> a) -> a -> a
lookupEnv name stack heap justS justH nothing =
  case lookup name stack of
    Just i -> justS i
    Nothing -> case lookup name heap of Just i -> justH i
                                        Nothing -> nothing

assignStackA :: Int -> CExpr -> CStatement
assignStackA i e = CSExpr $ CAssign spA (Just i) e

assignStackB :: Int -> CExpr -> CStatement
assignStackB i e = CSExpr $ CAssign spB (Just i) e

assignHeap :: Int -> CExpr -> CStatement
assignHeap i e = CSExpr $ CAssign hp (Just i) e

assignNode :: CExpr -> CStatement
assignNode e = CSExpr $ CAssign node Nothing e

{- Compiles function applications -}
compileAp :: [(Var, Int)] -> [Var] -> Var -> [Atom] -> [CStatement]
compileAp fVars args name as = [updateNode] ++ grabArgs ++ pushArgs ++ adjustSp
  ++ [enter]
  where -- A map of the function arguments to offsets from the stack pointer
        argsMap = zip args [0..]

        updateNode = CAnn ("Grab " ++ name ++ " into Node") $
          assignNode $ lookupEnv name argsMap fVars
          (CArrayElement spA) (CArrayElement node)
          (CCast pointerTD $ CID $ name ++ "_closure")
        enter = CAnn ("Enter " ++ name) $ CSEnter $ "(pointer**)" ++ node

        -- Grab all of the arguments into local variables.
        grabArgs = map grabArg args

        grabArg :: Var -> CStatement
        grabArg arg = CAnn ("Grab " ++ arg ++ " into a local variable") $
          CSVarDecl $ CVarDecl pointerTD ("t" ++ show i)
          Nothing (Just $ CArrayElement spA i)

          where Just i = lookup arg argsMap

        -- Partition the arguments to the new function into pointers and non-
        -- pointers
        (aVars, aLits) = partArgs as ([],[])
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
          assignStackA (length args - n) expr
          where expr = lookupEnv var argsMap fVars
                       (CID . ("t" ++) . show) (CArrayElement node)
                       (CCast pointerTD $ CID $ var ++ "_entry")

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
                         COp (op diff) (CID name) (CLit $ abs diff)]
        diffA = length args - numA
        diffB = numB
        op diff | diff > 0 = CPlus
                | otherwise = CMinus

compileLet :: [(Var,Int)] -> [Var] -> [STGBinding] -> STGExpr -> [CStatement]
compileLet letFVars letArgs binds expr = allocateHeap ++ fillClosures ++
  callExpr
  where
  -- A map of the function arguments to offsets from the stack pointer
  argsMap = zip letArgs [0..]

  -- Allocate enough space in the heap for the closures
  allocateHeap = [
    CAnn "Allocate some heap" $
    CSExpr $ CAssign hp Nothing $ COp CMinus (CID hp) (CLit spaceNeeded),
    CSIf (COp CLt (CID hp) (CID hLimit)) [] [] ]

  -- Fill in the closures
  (fillClosures, spaceNeeded) = first concat $
    runState (mapM fillClosure binds) 0

  -- Call the expression in the body of the let
  callExpr = []

  -- Fill in a heap-allocated closure
  fillClosure :: STGBinding -> State Int [CStatement]
  fillClosure bind@(STGBinding name LambdaForm{..}) = do
    -- Get the current heap offset
    i <- get

    let (infoTable,stdEntry) = convert bind
        assignInfoTable = assignHeap i (CID $ varName infoTable)

        fVarE fVar = lookupEnv fVar argsMap letFVars
          (CArrayElement spA) (CArrayElement node)
          (CCast pointerTD $ CID $ name ++ "_closure")
        assigns = assignInfoTable :
          zipWith (\i' fVar -> CAnn fVar $ assignHeap i' (fVarE fVar))
          [i+1..] (Set.toList fVars)

        comment = CComment $ "Fill in closure for " ++ name

    -- Update the heap offset
    put $ i + length assigns

    return $ CNested (Left stdEntry):CNested (Right infoTable):comment:assigns

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
  pp (CPointer name) = char '*' <> text name
  pp (CCall expr args) = parens (pp expr) <> parens (pp args)
  pp (CCast ty e) = parens (pp ty) <> pp e
  pp (CArray exprs) = encloseSep lbrace rbrace comma (map pp exprs)
  pp (CArrayElement arr i) = text arr <> brackets (int i)

instance Pretty COp where
  pp CEq = text "=="
  pp CNe = text "!="
  pp CLt = char '<'
  pp CPlus = char '+'
  pp CMinus = char '-'
