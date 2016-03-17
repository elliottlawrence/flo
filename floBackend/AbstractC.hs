{-# LANGUAGE MultiParamTypeClasses, RecordWildCards, TypeSynonymInstances,
    FlexibleInstances #-}
module AbstractC where

import Convertible
import Pretty
import STG

import Text.PrettyPrint.Leijen hiding (Pretty)

type ID = String

data CProgram = CProgram [CDeclString] [CVarDecl] [CFunction]

{- Top-level declarations can be complicated, so we'll just say they're
   strings.-}
type CDeclString = String

data CVarDecl = CVarDecl CType ID (Maybe Int) (Maybe CExpr)
data CFunction = CFunction CType ID [CParam] [CStatement]

data CType = CInt | CTypeDef String | CPointerType CType

data CParam = CParam CType ID

data CStatement = CSVarDecl CVarDecl
                | CSExpr CExpr
                | CSIf CExpr [CStatement] [CStatement]
                | CSWhile CExpr [CStatement]
                | CSReturn (Maybe CExpr)

data CExpr = CID ID
           | CAssign ID CExpr
           | COp COp CExpr CExpr
           | CLit Lit
           | CPointer ID
           | CCall CExpr [CExpr]
           | CCast CType CExpr

data COp = CEq | CNe | CPlus | CMinus

pointerTD :: CType
pointerTD = CTypeDef "pointer"

functionTD:: CType
functionTD = CTypeDef "function"

cMain :: CFunction
cMain = CFunction CInt "main" [] [f_main, cont, interpreter, ret]
  where f_main = CSVarDecl $ CVarDecl functionTD "f_main" Nothing
          (Just $ CCast functionTD (CID "main"))
        cont = CSVarDecl $ CVarDecl functionTD "cont" Nothing Nothing
        interpreter =  CSWhile (COp CNe (CID "cont") (CID "f_main"))
          [CSExpr $ CAssign "cont" $
           CCast functionTD (CCall (CPointer "cont") [])]
        ret = CSReturn $ Just $ CLit 0

instance Convertible STGProgram CProgram where
  convert STGProgram{..} = CProgram
    [stdio, enterMacro, jumpMacro, pointerTypeDef, functionTypeDef]
    [spB, spA, hLimit, hp]
    [cMain]
    where -- Random declarations
          stdio = "#include <stdio.h>"
          enterMacro = "#define ENTER(c)  JUMP(**c)"
          jumpMacro = "#define JUMP(lbl)  return((pointer) lbl)"
          pointerTypeDef = "typedef int * pointer;"
          functionTypeDef = "typedef pointer (* function)();"
          -- Stacks
          stackLimit = 10000
          spB = CVarDecl pointerTD "SpB" (Just stackLimit) Nothing
          spA = CVarDecl (CPointerType pointerTD) "SpA" Nothing
            (Just $ COp CPlus (CID "SpB") (CLit $ stackLimit - 1))
          -- Heap
          heapLimit = 10000
          hLimit = CVarDecl CInt "HLimit" Nothing (Just $ CLit heapLimit)
          hp = CVarDecl pointerTD "Hp" (Just heapLimit) Nothing

-- Pretty printing functions
instance Pretty CProgram where
  pp (CProgram declStrings vars functions) = vcat $ punctuate line
    [vcat (map text declStrings), pp vars, pp functions]

instance Pretty CVarDecl where
  pp (CVarDecl ty name size initializer) = pp ty <+> text name <> size' <>
    initializer' <> semi
    where size' = case size of Just n -> brackets $ int n
                               Nothing -> empty
          initializer' = case initializer of
                          Just expr -> space <> equals <+> pp expr
                          Nothing -> empty

{- Wrap a document in C-style braces -}
cBraces :: Doc -> Doc
cBraces doc = vcat [lbrace, indent 4 doc, rbrace]

instance Pretty CFunction where
  pp (CFunction ty name args body) = pp ty <+> text name <> parens (pp args)
    <+> cBraces (pp body)
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

instance Pretty CExpr where
  pp (CID name) = text name
  pp (COp op e1 e2) = pp e1 <+> pp op <+> pp e2
  pp (CAssign name expr) = text name <+> equals <+> pp expr
  pp (CLit lit) = pp lit
  pp (CPointer name) = char '*' <> text name
  pp (CCall expr args) = parens (pp expr) <> parens (pp args)
  pp (CCast ty e) = parens (pp ty) <> pp e

instance Pretty COp where
  pp CEq = text "=="
  pp CNe = text "!="
  pp CPlus = char '+'
  pp CMinus = char '-'
