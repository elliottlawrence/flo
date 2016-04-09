{-# LANGUAGE RecordWildCards, MultiParamTypeClasses, FlexibleInstances,
    TupleSections #-}
module FloProgram where

import Convertible
import FloGraph
import Pretty

import Control.Monad.Reader
import Data.Char (isUpper)
import Data.Either (partitionEithers)
import qualified Data.IntMap as IntMap
import Data.List (elemIndex, find)
import Data.Maybe (fromMaybe, isJust)
import Text.PrettyPrint.Leijen as L hiding (Pretty)
import Text.Regex.Posix

data Literal = LitInt Int | LitFloat Double | LitChar Char | LitString String
               deriving Eq

data Type = TypeCons Name [Type]

data FloExpr = FloLit Literal                       -- Literals
             | FloVar Name                          -- Functions, variables
             | FloCons Name                         -- Constructors
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

data FloProgram = FloProgram {
  fpDataConses :: [FloDataCons],
  fpDefs :: [FloDef]
}

{- Reader monads for storing the current box definition and data constructors -}
type RBoxDef a = Reader BoxDef a
type RDataConses a = Reader [FloDataCons] a
type RBoxDefDataConses a = Reader (BoxDef, [FloDataCons]) a

{- FloGraphs consist of modules, but FloPrograms do not, so we must flatten the
   box definitions and data constructors into a single "module". -}
instance Convertible FloGraph FloProgram where
  convert (FloGraph modules) = FloProgram dataConses defs
    where module' = Module "main" (concatMap mDefs modules)
          (dataConses, defs') = partitionEithers $ convert $ mDefs module'
          defs = map (`runReader` dataConses) defs'

instance Convertible BoxDef (Either FloDataCons (RDataConses FloDef)) where
  convert bd | hasBox "DataCons" bd = Left $ convert bd
             | otherwise = Right $ convert bd

{- Determines if a box definition contains a box with the given name -}
hasBox :: String -> BoxDef -> Bool
hasBox name BoxDef{..} = isJust $
  find (\bi -> bName bi == name) $ IntMap.elems (biMap boxes)

instance Convertible BoxDef FloDataCons where
  convert bd@BoxDef{..} = runReader
    (liftM2 (FloDataCons dataConsName) fields t) (bd,[] :: [FloDataCons])
    where bi = getConnectedBox bd endInput
          dataConsName = bName boxInterface
          (fields, t) | bName bi == "idMono" = (convert $ bInputs bi !! 1,
                        convert $ head $ bInputs bi)
                      | bName bi == "DataCons" = (convert endInput,
                        return $ TypeCons dataConsName [])

{- Converts "DataCons" into a list of types -}
instance Convertible Input (RBoxDefDataConses [Type]) where
  convert i = do
    (bd,dataConses) <- ask
    mapM (liftM (\(FloAnn t _) -> t) . convert) (bInputs $ getConnectedBox bd i)

{- The expression component of a box definition is the expression determined by
   the output box. In addition, if a box has local definitions, these are
   captured by wrapping the box definition in a let expression. -}
instance Convertible BoxDef (RDataConses FloDef) where
  convert bd@BoxDef{..} = do
    dataConses <- ask
    localDefs' <- mapM convert localDefs

    let BoxInterface{..} = boxInterface
        boxExpr = runReader (convert endInput) (bd, dataConses)

    return $ FloDef bName (map iName bInputs) $
      (if null localDefs' then id else FloLet localDefs') boxExpr


{- Converts the input in the given box definition to an expression -}
instance Convertible Input (RBoxDefDataConses FloExpr) where
  convert i = do
    (bd@BoxDef{..},dataConses) <- ask
    let Output{..} = getConnectedOutputUnsafe bd i
        bi@BoxInterface{..} = lookupUnsafe oParentID boxes

    -- Box definition input
    if oParentID == -1 then return $ FloVar oEndInputName

    -- Used for type annotations
    else if bName == "idMono" then
      liftM2 FloAnn (convert $ head bInputs) (convert $ bInputs !! 1)

    -- Case expressions
    else if bName == "case" then
      liftM2 FloCase (convert $ head bInputs)
        (liftM pairZip $ mapM convert (tail bInputs))

    -- Typical expressions
    else do
      let {- To create an expression out of a function or constructor, convert
             all of its inputs to expressions and then apply them to the
             function. For constant applicative forms, no inputs need be
             applied. -}
          createAp :: RBoxDefDataConses FloExpr
          createAp = if null unappliedInputs then rhs
                     else liftM (FloLambda unappliedInputs) rhs
            where unappliedInputs = concatMap
                    (mapApplied bd (const []) (replicate 1 . iName)) bInputs
                  rhs = do aps <- mapM (mapApplied bd convert
                                  (return . FloVar . iName)) bInputs
                           return $ foldl1 FloAp $ floVarCons bName : aps

          floVarCons :: Name -> FloExpr
          floVarCons name | name `elem` map dcName dataConses = FloCons name
                          | otherwise = FloVar name

      ap <- createAp
      return $ removeId $ fromMaybe ap (convert bi)

    where
      mapApplied :: BoxDef -> (Input -> b) -> (Input -> b) -> Input -> b
      mapApplied bd t f i = if isApplied bd i then t i else f i

      pairZip :: [a] -> [(a,a)]
      pairZip [] = []
      pairZip (x:y:xs) = (x,y) : pairZip xs

      {- A simple optimization that is also useful for simulating $. -}
      removeId :: FloExpr -> FloExpr
      removeId (FloAp (FloVar "id") e) = e
      removeId (FloAp e1 e2) = FloAp (removeId e1) e2
      removeId (FloLambda is e) = FloLambda is (removeId e)
      removeId e = e

{- Converts the input in the given box definition to a type -}
instance Convertible Input (RBoxDefDataConses Type) where
  convert t = do
    (bd,_) <- ask
    let BoxInterface{..} = getConnectedBox bd t
    liftM (TypeCons bName) (mapM convert bInputs)

{- A literal is simply the box's name. -}
instance Convertible BoxInterface (Maybe FloExpr) where
  convert BoxInterface{..}
    | isLitString bName = justLit $ LitString (init $ tail bName)
    | isLitChar bName = justLit $ LitChar (read bName)
    | isPrimInt bName = justLit $ LitInt (read $ init bName)
    | isLitInt bName = Just $
        FloAp (FloCons "MkInt") (FloLit $ LitInt $ read bName)
    | isPrimFloat bName = justLit $ LitFloat (read $ init bName)
    | isLitFloat bName = Just $
        FloAp (FloCons "MkFloat") (FloLit $ LitFloat $ read bName)
    | otherwise = Nothing
    where justLit = Just . FloLit

isLitString :: String -> Bool
isLitString = (=~ "^\".*\"$")   -- ^".*"$

isLitChar :: String -> Bool
isLitChar = (=~ "^'[^\\']'$|^'\\\\['trn]'$|^'\\\\\\\\'$")
              -- ^'[^\']'$|^'\\['trn]'$|^'\\\\'$

isLitInt :: String -> Bool
isLitInt = (=~ "^-?[0-9]+$")    -- ^-?[0-9]+$

isPrimInt :: String -> Bool
isPrimInt = (=~ "^-?[0-9]+\\$$")    -- ^-?[0-9]+\$$

isLitFloat :: String -> Bool
isLitFloat = (=~ "^-?[0-9]*\\.[0-9]+$")     -- ^-?[0-9]*\.[0-9]+$

isPrimFloat :: String -> Bool
isPrimFloat = (=~ "^-?[0-9]*\\.[0-9]+\\$$")     -- ^-?[0-9]*\.[0-9]+\$$

-- Pretty printing
instance Pretty Literal where
  pp (LitInt i) = pp i
  pp (LitFloat d) = pp d
  pp (LitChar c) = squotes $ char c
  pp (LitString s) = dquotes $ text s

instance Pretty FloExpr where
  pp (FloLit lit) = pp lit
  pp (FloVar name) = text name
  pp (FloCons name) = text name
  pp (FloAp e1 e2) = pp e1 <+> maybeParens (not $ isAtomicE e2) (pp e2)
  pp (FloLambda inputs expr) = char '\\' <> pp inputs <+> text "->" <+> pp expr
  pp (FloLet ld le) = text "let" <+> braces (vcat $ punctuate semi (map pp ld))
    </> text "in" <+> align (pp le)
  pp (FloCase e alts) = text "case" <+> pp e <+> text "of" </> align (pp alts)
  pp (FloAnn t e) = parens $ maybeParens (not $ isAtomicE e) (pp e) <+>
    text "::" <+> pp t

isAtomicE :: FloExpr -> Bool
isAtomicE (FloLit _) = True
isAtomicE (FloVar _) = True
isAtomicE (FloCons _) = True
isAtomicE _ = False

instance Pretty FloDef where
  pp FloDef{..} = hsep [text fdName, hsep $ map pp fdInputs, equals,
    align $ pp fdExpr]

instance Pretty FloDataCons where
  pp FloDataCons{..} = hsep [text "data", text dcName, pp dcFields, text "::",
    pp dcType]

instance Pretty Type where
  pp (TypeCons name vars) = text name <> vars'
    where vars' | null vars = L.empty
                | otherwise = space <> pp vars
  ppList = hsep . map (\t -> maybeParens (not $ isAtomicT t) (pp t))

instance Pretty (FloExpr, FloExpr) where
  pp (e1, e2) = pp e1 <+> text "->" <+> align (pp e2)

isAtomicT :: Type -> Bool
isAtomicT (TypeCons _ []) = True
isAtomicT _ = False

instance Pretty FloProgram where
  pp FloProgram{..} = pp fpDataConses <$$> pp fpDefs
