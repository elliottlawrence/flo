{-# LANGUAGE MultiParamTypeClasses, RecordWildCards #-}
module AbstractC where

import Convertible
import Pretty
import STG

import Text.PrettyPrint.Leijen hiding (Pretty)

type ID = String

data CProgram = CProgram [CMacro] [CVarDecl] [CFunction]

type CInclude = String

{- Macros are complicated, so we'll just say they're strings. -}
type CMacro = String

data CVarDecl = CVarDecl CType ID
data CFunction = CFunction CType ID [CParam] [CStatement]

data CType = CInt | CVoid

data CParam = CParam CType ID

data CStatement = CSVarDecl CVarDecl
                | CSExpr CExpr
                | CSIf CExpr [CStatement] [CStatement]
                | CSWhile CExpr [CStatement]
                | CSReturn (Maybe CExpr)

data CExpr = CAssign ID CExpr
           | COp COp CExpr CExpr
           | CLit CLit
           | CPointer ID
           | CCall CExpr [CExpr]

data COp = CEq

data CLit = CBool Bool

cMain :: CFunction
cMain = CFunction CInt "main" [] [interpreter]

interpreter :: CStatement
interpreter = CSWhile (CLit $ CBool True)
  [CSExpr $ CAssign "cont" (CCall (CPointer "cont") [])]

instance Convertible STGProgram CProgram where
  convert STGProgram{..} = CProgram
    [trueFalseMacro, enterMacro, jumpMacro] [] [cMain]
    where trueFalseMacro = "#define TRUE  1\n#define FALSE 0"
          jumpMacro = "#define JUMP(lbl)  return(lbl)"
          enterMacro = "#define ENTER(c)  JUMP(**c)"

-- Pretty printing functions
instance Pretty CProgram where
  pp (CProgram macros vars functions) =
    --vcat (map ((text "#include" <+>) . text) includes) <$$>
    vcat (map text macros) <$$> empty <$$>
    pp vars <$$> pp functions

instance Pretty CVarDecl where
  pp (CVarDecl ty name) = pp ty <+> text name <> semi

{- Wrap a document in C-style braces -}
cBraces :: Doc -> Doc
cBraces doc = vcat [lbrace, indent 4 doc, rbrace]

instance Pretty CFunction where
  pp (CFunction ty name args body) = pp ty <+> text name <+> parens (pp args)
    <+> cBraces (pp body)
  ppList = vcat . punctuate line . map pp

instance Pretty CType where
  pp CInt = text "int"
  pp CVoid = text "void"

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
  pp (CAssign name expr) = text name <+> equals <+> pp expr
  pp (CLit lit) = pp lit
  pp (CPointer name) = char '*' <> text name
  pp (CCall expr args) = parens (pp expr) <> parens (pp args)

instance Pretty CLit where
  pp (CBool bool) | bool = text "TRUE"
                  | otherwise = text "FALSE"
