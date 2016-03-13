{-# LANGUAGE RecordWildCards, MultiParamTypeClasses, FlexibleInstances,
    TupleSections #-}
module FloProgram where

import Convertible
import FloGraph
import Pretty

import Control.Monad.Reader
import Data.Char (isUpper)
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

{- A reader monad for storing the current box definition -}
type RBoxDef a = Reader BoxDef a

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
  find (\bi -> bName bi == name) $ IntMap.elems (biMap boxes)

instance Convertible BoxDef FloDataCons where
  convert bd@BoxDef{..} =
    FloDataCons dataConsName (runReader fields bd) (runReader t bd)
    where bi = getConnectedBox bd endInput
          dataConsName = bName boxInterface
          (fields, t) | bName bi == "idMono" = (convert $ bInputs bi !! 1,
                        convert $ head $ bInputs bi)
                      | bName bi == "DataCons" = (convert endInput,
                        return $ TypeCons dataConsName [])

{- Converts "DataCons" into a list of types -}
instance Convertible Input (RBoxDef [Type]) where
  convert i = do
    bd <- ask
    mapM (liftM (\(FloAnn t _) -> t) . convert) (bInputs $ getConnectedBox bd i)

{- The expression component of a box definition is the expression determined by
   the output box. In addition, if a box has local definitions, these are
   captured by wrapping the box definition in a let expression. -}
instance Convertible BoxDef FloDef where
  convert bd@BoxDef{..} = FloDef bName (map iName bInputs) $
    if null localDefs then boxExpr
                      else FloLet (map convert localDefs) boxExpr
    where BoxInterface{..} = boxInterface
          boxExpr = runReader (convert endInput) bd

{- Converts the input in the given box definition to an expression -}
instance Convertible Input (RBoxDef FloExpr) where
  convert i = do
    bd@BoxDef{..} <- ask
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
          createAp :: RBoxDef FloExpr
          createAp = do
            bd <- ask
            let unappliedInputs = concatMap
                    (mapApplied bd (const []) (replicate 1 . iName)) bInputs
                rhs = do aps <- mapM (mapApplied bd convert
                                (return . FloVar . iName)) bInputs
                         return $ foldl1 FloAp $
                          floVarCons bName (length bInputs) : aps
            if null unappliedInputs then rhs
            else liftM (FloLambda unappliedInputs) rhs

      ap <- createAp
      return $ removeId $ fromMaybe ap (convert bi)

    where
      mapApplied :: BoxDef -> (Input -> b) -> (Input -> b) -> Input -> b
      mapApplied bd t f i = if isApplied bd i then t i else f i

      pairZip :: [a] -> [(a,a)]
      pairZip [] = []
      pairZip (x:y:xs) = (x,y) : pairZip xs

      {- By convention, data constructors start with capital letters. -}
      floVarCons :: Name -> Int -> FloExpr
      floVarCons name i | isUpper n = FloCons name i
                        | otherwise = FloVar name
        where name'@(n:ns) = case elemIndex '.' name of
                              Just i -> drop (i+1) name
                              Nothing -> name

      {- A simple optimization that is also useful for simulating $. -}
      removeId :: FloExpr -> FloExpr
      removeId (FloAp (FloVar "id") e) = e
      removeId (FloAp e1 e2) = FloAp (removeId e1) e2
      removeId (FloLambda is e) = FloLambda is (removeId e)
      removeId e = e

{- Converts the input in the given box definition to a type -}
instance Convertible Input (RBoxDef Type) where
  convert t = do
    bd <- ask
    let BoxInterface{..} = getConnectedBox bd t
    liftM (TypeCons bName) (mapM convert bInputs)

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
  pp (LitInt i) = pp i
  pp (LitFloat d) = pp d
  pp (LitChar c) = squotes $ char c
  pp (LitString s) = dquotes $ text s

instance Pretty FloExpr where
  pp (FloLit lit) = pp lit
  pp (FloVar name) = text name
  pp (FloCons name _) = text name
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
isAtomicE (FloCons _ _) = True
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
                | otherwise = L.empty <+> pp vars
  ppList = hsep . map (\t -> maybeParens (not $ isAtomicT t) (pp t))

instance Pretty (FloExpr, FloExpr) where
  pp (e1, e2) = pp e1 <+> text "->" <+> align (pp e2)

isAtomicT :: Type -> Bool
isAtomicT (TypeCons _ []) = True
isAtomicT _ = False

instance Pretty FloDecl where
  pp (FD fd) = pp fd
  pp (DC dc) = pp dc

instance Pretty FloModule where
  pp FloModule{..} = text "FloModule" <+> text fmName <+> lbrace <$$>
    indent 4 (pp fmDecls) <$$> rbrace
