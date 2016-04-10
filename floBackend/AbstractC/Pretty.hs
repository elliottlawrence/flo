{-# LANGUAGE RecordWildCards #-}
module AbstractC.Pretty where

import AbstractC.Base
import Pretty

import Text.PrettyPrint.Leijen hiding (Pretty)

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
  pp CIntP = text "intptr_t"
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
  pp (CComment comment)
    | null comment = empty
    | otherwise = enclose (text "/* ") (text " */") (pp comment)
  pp (CAnn comment statement) = fill 30 (pp statement) <+>
    enclose (text "/* ") (text " */") (pp comment)

instance Pretty CCase where
  pp (CCase (-1) body) = text "default" <> colon <$$>
    cBraces (pp body <$$> text "break" <> semi)
  pp (CCase i body) = text "case" <+> int i <> colon <$$>
    cBraces (pp body <$$> text "break" <> semi)

instance Pretty CExpr where
  pp (CID name) = text name
  pp (COp op e1 e2) = pp e1 <+> pp op <+> pp e2
  pp (CAssign name index expr) = text name <> bracks' <+> equals <+> pp expr
    where bracks' = case index of Just i -> brackets $ int i
                                  Nothing -> empty
  pp (CLit lit) = pp lit
  pp (CString string) = dquotes $ text string
  pp (CPointer expr) = char '*' <> pp expr
  pp (CCall expr args) = pp expr <> parens (commas $ map pp args)
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
  pp CMult = char '*'
  pp CDiv = char '/'
  pp CMod = char '%'
