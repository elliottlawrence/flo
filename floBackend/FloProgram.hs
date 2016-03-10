{-# LANGUAGE RecordWildCards, MultiParamTypeClasses, FlexibleInstances,
    TupleSections #-}
module FloProgram where

import Convertible
import FloGraph
import Pretty

import Data.Char (isUpper)
import qualified Data.IntMap as IntMap
import Data.List (find)
import Data.Maybe (fromMaybe, isJust)
import Text.PrettyPrint
import Text.Regex.Posix

data Literal = LitInt Int | LitFloat Double | LitChar Char | LitString String
               deriving Eq

data Type = TypeCons Name [Type]

data FloExpr = FloLit Literal                       -- Literals
             | FloVar Name                          -- Functions, variables
             | FloCons Name Int                     -- Constructors (arity)
             | FloAp FloExpr FloExpr                -- Applications
             | FloLambda [Name] FloExpr             -- Lambdas
             | FloLet [FloDef] FloExpr              -- Let expressions
             | FloCase FloExpr [(FloExpr, FloExpr)] -- Case expressions
             | FloAnn Type FloExpr                  -- Type annotations

data FloDef = FloDef {
  fdName :: Name,
  fdInputs :: [Name],
  fdExpr :: FloExpr
}

data FloDataCons = FloDataCons {                  -- Data constructors
  dcName :: Name,
  dcFields :: [Type],
  dcType :: Type
}

{- Top-level declarations may be either function definitions or data constructor
   definitions -}
data FloDecl = FD FloDef | DC FloDataCons

data FloModule = FloModule {
  fmName :: Name,
  fmDecls :: [FloDecl]
}

type FloProgram = [FloModule]

instance Convertible FloGraph FloProgram where
  convert (FloGraph modules) = convert modules

instance Convertible Module FloModule where
  convert Module{..} = FloModule mName $ convert mDefs

instance Convertible BoxDef FloDecl where
  convert bd | hasBox "DataCons" bd = DC $ convert bd
             | otherwise = FD $ convert bd

{- Determines if a box definition contains a box with the given name -}
hasBox :: String -> BoxDef -> Bool
hasBox name BoxDef{..} = isJust $
  find (\bi -> bName bi == name) $ IntMap.elems boxes

instance Convertible BoxDef FloDataCons where
  convert bd@BoxDef{..} = FloDataCons dataConsName fields t
    where bi = getConnectedBox bd endInput
          dataConsName = bName boxInterface
          (fields, t) | bName bi == "idMono" = (convert (bd, bInputs bi !! 1),
                                                convert (bd, head $ bInputs bi))
                      | bName bi == "DataCons" = (convert (bd, endInput),
                                                  TypeCons dataConsName [])

{- Converts "DataCons" into a list of types -}
instance Convertible (BoxDef, Input) [Type] where
  convert (bd@BoxDef{..}, i) = map (convertAnn . convert . (bd,)) bInputs
    where BoxInterface{..} = getConnectedBox bd i
          convertAnn (FloAnn t _) = t

{- The expression component of a box definition is the expression determined by
   the output box. In addition, if a box has local definitions, these are
   captured by wrapping the box definition in a let expression. -}
instance Convertible BoxDef FloDef where
  convert bd@BoxDef{..} = FloDef bName (map iName bInputs) $
    if null localDefs then boxExpr
                      else FloLet (map convert localDefs) boxExpr
    where BoxInterface{..} = boxInterface
          boxExpr = convert (bd, endInput)

{- Converts the input in the given box definition to an expression -}
instance Convertible (BoxDef, Input) FloExpr where
  convert (bd@BoxDef{..}, i)
    | oParentID == -1 = FloVar oEndInputName
    -- Used for type annotations
    | bName == "idMono" = FloAnn (convert (bd, head bInputs))
      (convert (bd, bInputs !! 1))
    -- Case expressions
    | bName == "case" = FloCase (convert (bd, head bInputs)) $
      pairZip $ map (convert . (bd,)) (tail bInputs)
    -- Typical expressions
    | otherwise = removeId $ fromMaybe createAp (convert bi)
    where Output{..} = getConnectedOutputUnsafe bd i
          bi@BoxInterface{..} = lookupUnsafe oParentID boxes

          pairZip :: [a] -> [(a,a)]
          pairZip [] = []
          pairZip (x:y:xs) = (x,y) : pairZip xs

          {- To create an expression out of a function or constructor, convert
             all of its inputs to expressions and then apply them to the
             function. For constant applicative forms, no inputs need be
             applied. -}
          createAp :: FloExpr
          createAp | null unappliedInputs = rhs
                   | otherwise = FloLambda unappliedInputs rhs
            where unappliedInputs = concatMap
                    (mapApplied (const []) (replicate 1 . iName)) bInputs
                  rhs = foldl1 FloAp $ floVarCons bName (length bInputs) :
                    map (mapApplied (convert . (bd,)) (FloVar . iName)) bInputs

          mapApplied :: (Input -> b) -> (Input -> b) -> Input -> b
          mapApplied t f i = if isApplied bd i then t i else f i

          {- By convention, data constructors start with capital letters. -}
          floVarCons :: Name -> Int -> FloExpr
          floVarCons name@(n:ns) i | isUpper n = FloCons name i
                                   | otherwise = FloVar name

          {- A simple optimization that is also useful for simulating $. -}
          removeId :: FloExpr -> FloExpr
          removeId (FloAp (FloVar "id") e) = e
          removeId (FloAp e1 e2) = FloAp (removeId e1) e2
          removeId (FloLambda is e) = FloLambda is (removeId e)
          removeId e = e

{- Converts the input in the given box definition to a type -}
instance Convertible (BoxDef, Input) Type where
  convert (bd@BoxDef{..}, t) = TypeCons bName $ map (convert . (bd,)) bInputs
    where BoxInterface{..} = getConnectedBox bd t

{- A literal is simply the box's name. -}
instance Convertible BoxInterface (Maybe FloExpr) where
  convert BoxInterface{..}
    | isLitString bName = justLit $ LitString (init $ tail bName)
    | isLitChar bName = justLit $ LitChar (read bName)
    | isLitInt bName = justLit $ LitInt (read bName)
    | isLitFloat bName = justLit $ LitFloat (read bName)
    | otherwise = Nothing
    where justLit = Just . FloLit

isLitString :: String -> Bool
isLitString = (=~ "^\".*\"$")   -- ^".*"$

isLitChar :: String -> Bool
isLitChar = (=~ "^'[^\\']'$|^'\\\\['trn]'$|^'\\\\\\\\'$")
              -- ^'[^\']'$|^'\\['trn]'$|^'\\\\'$

isLitInt :: String -> Bool
isLitInt = (=~ "^[0-9]+$")    -- ^[0-9]+$

isLitFloat :: String -> Bool
isLitFloat = (=~ "^[0-9]*\\.[0-9]+$")     -- ^[0-9]*\.[0-9]+$

-- Pretty printing
instance Pretty Literal where
  pp (LitInt i) = int i
  pp (LitFloat d) = double d
  pp (LitChar c) = quotes $ char c
  pp (LitString s) = doubleQuotes $ text s

instance Pretty FloExpr where
  pp (FloLit lit) = pp lit
  pp (FloVar name) = text name
  pp (FloCons name _) = text name
  pp (FloAp e1 e2) = pp e1 <+> e2'
    where e2' | isAtomicE e2 = pp e2
              | otherwise = parens $ pp e2
  pp (FloLambda inputs expr) = char '\\' <> pp inputs <+> text "->" <+> pp expr
  pp (FloLet ld le) = text "let" <+> braces (vcat $ punctuate semi (map pp ld))
    <+> text "in" $$ pp le
  pp (FloCase e alts) = text "case" <+> pp e <+> text "of" <+>
    vcat (map pp alts)
  pp (FloAnn t e) = parens $ e' <+> text "::" <+> pp t
    where e' | isAtomicE e = pp e
             | otherwise = parens $ pp e

isAtomicE :: FloExpr -> Bool
isAtomicE (FloLit _) = True
isAtomicE (FloVar _) = True
isAtomicE (FloCons _ _) = True
isAtomicE _ = False

instance Pretty [Name] where
  pp = hsep . map text

instance Pretty FloDef where
  pp FloDef{..} = text fdName <+> pp fdInputs <+> equals <+> nest 4 (pp fdExpr)

instance Pretty FloDataCons where
  pp FloDataCons{..} = text "data" <+> text dcName <+> pp dcFields <+>
    text "::" <+> pp dcType

instance Pretty Type where
  pp (TypeCons name vars) = text name <+> pp vars

instance Pretty [Type] where
  pp = hsep . map (\t -> if isAtomicT t then pp t else parens $ pp t)

instance Pretty (FloExpr, FloExpr) where
  pp (e1, e2) = pp e1 <+> text "->" <+> pp e2

isAtomicT :: Type -> Bool
isAtomicT (TypeCons _ []) = True
isAtomicT _ = False

instance Pretty FloDecl where
  pp (FD fd) = pp fd
  pp (DC dc) = pp dc

instance Pretty FloModule where
  pp FloModule{..} = text "FloModule" <+> text fmName <+> lbrace $$
    nest 4 (vcat (map pp fmDecls) $$ rbrace)

instance Pretty FloProgram where
  pp = vcat . map pp
