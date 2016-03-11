{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses,
    RecordWildCards #-}
module STG where

import Convertible
import FloProgram
import Pretty

import Control.Monad.Reader
import Data.List ((\\))
import Data.Maybe (catMaybes)
import qualified Data.Set as Set
import Text.PrettyPrint

data STGProgram = STGProgram [STGBinding]

data STGBinding = STGBinding Var LambdaForm

{- A set of global or free variables -}
type Bindings = Set.Set Var

{- A reader monad for storing the global variables -}
type RBinds a = Reader Bindings a

data LambdaForm = LambdaForm {
  fVars :: Bindings,
  flag :: UpdateFlag,
  args :: [Var],
  expr :: STGExpr
}

data UpdateFlag = U     -- Updatable
                | N     -- Not updatable

type Rec = Bool

data STGExpr = STGLet Rec [STGBinding] STGExpr      -- Local definition
             | STGCase STGExpr STGAlts              -- Case expression
             | STGAp Var [Atom]                     -- Application
             | STGCons Cons [Atom]                  -- Constructor
             | STGPrim Var Atom Atom                -- Built-in operator
             | STGLit Lit                           -- Literals
             | STGLambda LambdaForm                 -- Lambdas

data STGAlts = STGAAlts [STGAAlt] (Maybe STGDAlt)   -- Algebraic
             | STGPAlts [STGPAlt] (Maybe STGDAlt)   -- Primitive

data STGAAlt = STGAAlt Cons [Var] STGExpr           -- Algebraic alt
data STGPAlt = STGPAlt Lit STGExpr                  -- Primitive alt
data STGDAlt = STGDAlt1 Var STGExpr                 -- Default alt
             | STGDAlt2 STGExpr

{- So far, only integer literals are supported. -}
type Lit = Int

data Atom = AtomVar Var | AtomLit Lit

type Var = String
type Cons = String

{- The primitive binary operations. -}
primOps :: [String]
primOps = ["+", "-", "*", "/"]

{- Finds the free variables in an expression, atom, binding, etc. -}
class FreeVars a where
  free :: a -> RBinds Bindings

instance FreeVars a => FreeVars [a] where
  free as = mapM free as >>= \frees -> return $ Set.unions frees

instance (FreeVars a, FreeVars b) => FreeVars (a,b) where
  free (a,b) = do a' <- free a
                  b' <- free b
                  return $ Set.union a' b'

instance FreeVars a => FreeVars (Maybe a) where
  free (Just a) = free a
  free Nothing = return Set.empty

instance FreeVars Atom where
  free (AtomVar v) = do
    contains <- asks (v `elem`)
    return $ if contains then Set.empty else Set.singleton v
  free (AtomLit _) = return Set.empty

instance FreeVars STGExpr where
  free (STGLit _) = return Set.empty
  free (STGPrim _ a1 a2) = free [a1,a2]
  free (STGCons _ as) = free as
  free (STGAp v as) = free (AtomVar v, as)
  free (STGLet _ binds e) = do
    bindse' <- free (binds,e)
    let bound = Set.fromList $ map (\(STGBinding var _) -> var) binds
    return $ bindse' Set.\\ bound
  free (STGCase e alts) = free (e,alts)
  free (STGLambda lf) = free lf

instance FreeVars LambdaForm where
  -- We will always assume that a lambda form's list of free variables is
  -- correct.
  free LambdaForm{..} = return fVars

instance FreeVars STGBinding where
  free (STGBinding var lform) = do
    lform' <- free lform
    return $ Set.delete var lform'

instance FreeVars STGAlts where
  free (STGAAlts aalts def) = free (aalts,def)
  free (STGPAlts palts def) = free (palts,def)

instance FreeVars STGAAlt where
  free (STGAAlt _ vars e) = do
    e' <- free e
    return $ e' Set.\\ Set.fromList vars

instance FreeVars STGPAlt where
  free (STGPAlt _ e) = free e

instance FreeVars STGDAlt where
  free (STGDAlt1 var e) = liftM (Set.delete var) (free e)
  free (STGDAlt2 e) = free e

{- Creates a lambda form complete with free variables and update flag -}
createLForm :: [Var] -> STGExpr -> RBinds LambdaForm
createLForm args expr = do
  globs <- ask
  expr' <- free expr
  let fVars = (expr' Set.\\ Set.fromList args) Set.\\ globs
  -- Add redundant free variables to ensure that constructors are standard
  fVars' <- case expr of
              STGCons _ atoms -> do atoms' <- free atoms
                                    return $ fVars `Set.union` atoms'
              _ -> return fVars
  return $ LambdaForm fVars' (getFlag args expr) args expr

  where getFlag (a:as) _ = N  -- Manifest functions
        getFlag [] (STGCons _ _) = N  -- Constructors
        getFlag [] _ = U  -- Thunks/default

{- Converts a FloProgram to an STGProgram -}
instance Convertible FloProgram STGProgram where
  convert modules = STGProgram $ runReader (mapM convert defs) globals
          -- Flatten the modules into a single list of declarations (for now)
    where defs = concatMap (\d -> case d of FD def -> [def]; DC _ -> [])
                 (concatMap fmDecls modules)
          -- The globals are the top level definitions
          globals = Set.fromList $ map fdName defs

{- Each flo definition corresponds to an STG binding. -}
instance Convertible FloDef (RBinds STGBinding) where
  convert FloDef{..} = do expr <- convert fdExpr
                          liftM (STGBinding fdName) (createLForm fdInputs expr)

instance Convertible FloExpr (RBinds STGExpr) where
  -- Only integer literals are supported at the moment
  convert (FloLit (LitInt i)) = return $ STGLit i

  convert (FloVar n)
    -- Case B1: Operator has no arguments
    | n `elem` primOps = liftM STGLambda $
      createLForm [a1,a2] $ STGPrim n (AtomVar a1) (AtomVar a2)
    | otherwise = return $ STGAp n []
    where [a1,a2] = take 2 $ newArgs "_"

  convert (FloCons n i)
    -- Case C1: Constructor takes no arguments
    | i == 0 = return $ STGCons n []
    -- Case C2: Constructor takes arguments, but none have been applied
    | i > 0 = liftM STGLambda $ createLForm args expr
    where args = take i $ newArgs "_"
          expr = STGCons n (map AtomVar args)

  convert aps@(FloAp e1 e2) = do
    let e:es = flatten aps
        retLetF letb lete = return $ maybeLet False letb lete
    (AtomVar av : atoms, binds) <- argsToAtomsBinds (e:es)

    case e of
    -- Note: If e is a constructor, argsToAtomsBinds will create a binding
    -- for it, so we must ignore it.
      FloCons n i
        -- Case C3: Constructor takes arguments, and all have been applied
        | i == length es -> retLetF (tail binds) (STGCons n atoms)
        -- Case C4: Constructor takes arguments, and some have been
        -- applied
        | otherwise -> do
          lForm <- createLForm args lexp
          retLetF (tail binds) (STGLambda lForm)
        where args = take (i - length es) $ newArgs "_"
              lexp = STGCons n $ atoms ++ map AtomVar args
      FloVar n
        -- Case B2: Operator has 1 argument
        | n `elem` primOps && length es == 1 -> do
          lForm <- createLForm [arg] lexp
          retLetF binds (STGLambda lForm)
        -- Case B3: Operator has 2 arguments
        | n `elem` primOps && length es == 2 ->
          retLetF binds (STGPrim n (head atoms) (atoms !! 1))
        where [arg] = take 1 $ newArgs "_"
              lexp = STGPrim n (head atoms) (AtomVar arg)
      -- Default case
      otherwise -> retLetF binds (STGAp av atoms)

  convert (FloLambda ns e) = do
    e' <- convert e
    liftM STGLambda $ createLForm ns e'

  convert (FloLet defs e) = liftM2 (STGLet True) (mapM convert defs) (convert e)

  convert (FloCase e alts) = liftM2 STGCase (convert e) (convert alts)

  convert (FloAnn _ e) = convert e

{- Flatten out a bunch of binary applications. -}
flatten :: FloExpr -> [FloExpr]
flatten (FloAp e1 e2) = flatten e1 ++ [e2]
flatten e = [e]

{- An infinite list of arguments with the given prefix -}
newArgs :: String -> [String]
newArgs prefix = map (\num -> prefix ++ show num) [1..]

{- Converts a list of expressions (function arguments) to a list of atomic
   expressions and bindings -}
argsToAtomsBinds :: [FloExpr] -> RBinds ([Atom], [STGBinding])
argsToAtomsBinds es = do
  (atoms, maybeBinds) <- mapAndUnzipM toAtomBind (zip es [1..])
  return (atoms, catMaybes maybeBinds)
  where
    toAtomBind :: (FloExpr, Int) -> RBinds (Atom, Maybe STGBinding)
    toAtomBind (e,num)
      | isAtomic e = return (toAtom e, Nothing)
      | otherwise = do
        let var = "__" ++ show num
        lForm <- join $ liftM (createLForm []) (convert e)
        return (AtomVar var, Just $ STGBinding var lForm)

    {- This is slightly different from isAtomicE, since for the purposes
       of adding parentheses, constructors are atomic, but for STG, they
       aren't. -}
    isAtomic :: FloExpr -> Bool
    isAtomic (FloVar _) = True
    isAtomic (FloLit _) = True
    isAtomic _ = False

    toAtom :: FloExpr -> Atom
    toAtom (FloVar n) = AtomVar n
    toAtom (FloLit (LitInt i)) = AtomLit i

{- Creates a let expression if the list of bindings is not empty. -}
maybeLet :: Bool -> [STGBinding] -> STGExpr -> STGExpr
maybeLet _ [] = id
maybeLet rec binds = STGLet rec binds

{- Convenient data type for the different kinds of alts -}
data Alt = AAlt STGAAlt | PAlt STGPAlt | DAlt STGDAlt

instance Convertible [(FloExpr, FloExpr)] (RBinds STGAlts) where
  convert alts = do
    alts' <- mapM convert alts

    -- Split the alts into regular alts and a default alt, if one exists
    let (def, alts'') = case last alts' of
                          DAlt dalt -> (Just dalt, init alts')
                          _ -> (Nothing, alts')

    -- If the only alt is a default, just return that
    if null alts'' then return $ STGAAlts [] def
    -- Assume the alts are either all algebraic or all primitive
    else return $ (case head alts'' of
          AAlt _ -> STGAAlts . map (\(AAlt aalt) -> aalt)
          PAlt _ -> STGPAlts . map (\(PAlt palt) -> palt)) alts'' def

{- Converts a patt -> expr pair into an STG alt. -}
instance Convertible (FloExpr, FloExpr) (RBinds Alt) where
  convert (patt, expr) = liftM
    (case flatten patt of
      FloCons cons _ : exprs -> AAlt . STGAAlt cons vars
        where vars = map (\(FloVar var) -> var) exprs
      [FloLit (LitInt i)] -> PAlt . STGPAlt i
      [FloVar var] -> DAlt . STGDAlt1 var
      _ -> DAlt . STGDAlt2) (convert expr)

-- Pretty printing
instance Pretty STGProgram where
  pp (STGProgram binds) = vcat $ map pp binds

instance Pretty STGBinding where
  pp (STGBinding n lf) = text n <+> equals <+> nest 4 (pp lf)

instance Pretty LambdaForm where
  pp LambdaForm{..} = fsep [braces (commas' $ map text (Set.toList fVars)) <+>
    text "\\" <> pp flag <+> braces (commas' $ map text args) <+> text "->",
    pp expr]

instance Pretty UpdateFlag where
  pp U = text "u"
  pp N = text "n"

instance Pretty STGExpr where
  pp (STGLet rec binds expr) = text "let" <> rec' <+> vcat (map pp binds) $$
    text "in" <+> pp expr
    where rec' = text $ if rec then "rec" else ""
  pp (STGCase expr alts) = text "case" <+> pp expr <+> text "of" $$
    nest 2 (pp alts)
  pp (STGAp var []) = text var
  pp (STGAp var atoms) = text var <+> braces (commas' $ map pp atoms)
  pp (STGCons cons atoms) = text cons <+> braces (commas' $ map pp atoms)
  pp (STGPrim var a1 a2) = text var <+> braces (commas' [pp a1, pp a2])
  pp (STGLit lit) = int lit
  pp (STGLambda lform) = pp lform

instance Pretty STGAlts where
  pp (STGAAlts aalts dalt) = vcat (map pp aalts) $$ pp dalt
  pp (STGPAlts palts dalt) = vcat (map pp palts) $$ pp dalt

instance Pretty STGAAlt where
  pp (STGAAlt cons vars expr) = text cons <+> braces (commas' $ map text vars)
    <+> text "->" <+> pp expr

instance Pretty STGPAlt where
  pp (STGPAlt lit expr) = int lit <+> text "->" <+> pp expr

instance Pretty STGDAlt where
  pp (STGDAlt1 var expr) = text var <+> text "->" <+> pp expr
  pp (STGDAlt2 expr) = text "default" <+> text "->" <+> pp expr

instance Pretty Atom where
  pp (AtomVar var) = text var
  pp (AtomLit lit) = int lit
