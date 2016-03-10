{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses,
    RecordWildCards #-}
module STG where

import Convertible
import FloProgram
import Pretty

import Data.Maybe (catMaybes)
import Data.List ((\\))
import Text.PrettyPrint

import Debug.Trace

data STGProgram = STGProgram [STGBinding]

data STGBinding = STGBinding Var LambdaForm

data LambdaForm = LambdaForm {
  fVars :: [Var],
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

data STGAlts = STGAAlts [STGAAlt] --STGDAlt         -- Algebraic
             | STGPAlts [STGPAlt] --STGDAlt         -- Primitive

data STGAAlt = STGAAlt Cons [Var] STGExpr           -- Algebraic alt
data STGPAlt = STGPAlt Lit STGExpr                  -- Primitive alt
--data STGDAlt = STGDAlt1 Var STGExpr               -- Default alt
--             | STGDAlt2 STGExpr

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
  free :: [String] -> a -> [Var]

instance FreeVars a => FreeVars [a] where
  free globs = concatMap (free globs)

instance FreeVars Atom where
  free globs (AtomVar v) | v `notElem` globs = [v]
                         | otherwise = []
  free _ (AtomLit _) = []

instance FreeVars STGExpr where
  free _ (STGLit _) = []
  free globs (STGPrim _ a1 a2) = free globs [a1,a2]
  free globs (STGCons _ as) = free globs as
  free globs (STGAp v as) = free globs as ++ v'
    where v' | v `notElem` globs = [v]
             | otherwise = []
  free globs (STGLet _ binds e) = undefined
  free globs (STGCase e alts) = free globs e ++ free globs alts
  free globs (STGLambda LambdaForm{..}) = (fVars \\ args) \\ globs

instance FreeVars STGAlts where
  free globs (STGAAlts aalts) = free globs aalts
  free globs (STGPAlts palts) = free globs palts

instance FreeVars STGAAlt where
  free globs (STGAAlt cons vars e) = undefined

instance FreeVars STGPAlt where
  free globs (STGPAlt lit e) = undefined

instance Convertible FloProgram STGProgram where
  convert modules = STGProgram $ convert defs
    -- Flatten the modules into a single list of declarations (for now)
    where defs = concatMap (\d -> case d of FD def -> [def]; DC _ -> [])
                 (concatMap fmDecls modules)

{- Each flo definition corresponds to an STG binding. -}
instance Convertible FloDef STGBinding where
  convert FloDef{..} = STGBinding fdName $
    LambdaForm fVars N fdInputs (convert fdExpr)
    where fVars = []

instance Convertible FloExpr STGExpr where
  -- Only integer literals are supported at the moment
  convert (FloLit (LitInt i)) = STGLit i
  convert (FloVar n)
    -- Case B1: Operator has no arguments
    | n `elem` primOps = STGLambda $ LambdaForm [] N [a1,a2] $
      STGPrim n (AtomVar a1) (AtomVar a2)
    | otherwise = STGAp n []
    where [a1,a2] = take 2 $ newArgs "_"
  convert (FloCons n i)
    -- Case C1: Constructor takes no arguments
    | i == 0 = STGCons n []
    -- Case C2: Constructor takes arguments, but none have been applied
    | i > 0 = STGLambda $ LambdaForm [] N args expr
    where args = take i $ newArgs "_"
          expr = STGCons n (map AtomVar args)
  convert aps@(FloAp e1 e2) = maybeLet False binds' expr
    where e:es = flatten aps
          (AtomVar av : atoms, binds) = argsToAtomsBinds (e:es)

          -- Note: If e is a constructor, argsToAtomsBinds will create a binding
          -- for it, so we must ignore it.
          (binds', expr) = case e of
            FloCons n i
              -- Case C3: Constructor takes arguments, and all have been applied
              | i == length es -> (tail binds, STGCons n atoms)
              -- Case C4: Constructor takes arguments, and some have been
              -- applied
              | otherwise -> (tail binds, STGLambda $ LambdaForm [] N args lexp)
              where args = take (i - length es) $ newArgs "_"
                    lexp = STGCons n $ atoms ++ map AtomVar args
            FloVar n
              -- Case B2: Operator has 1 argument
              | n `elem` primOps && length es == 1 -> (binds, STGLambda $
                LambdaForm [] N [arg] $ STGPrim n (head atoms) (AtomVar arg))
              -- Case B3: Operator has 2 arguments
              | n `elem` primOps && length es == 2 ->
                (binds, STGPrim n (head atoms) (atoms !! 1))
              where [arg] = take 1 $ newArgs "_"
            otherwise -> (binds, STGAp av atoms)

  convert (FloLambda ns e) = STGLambda $ LambdaForm [] N ns (convert e)
  convert (FloLet defs e) = STGLet True (convert defs) (convert e)
  convert (FloCase e alts) = STGCase (convert e) (convert alts)
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
argsToAtomsBinds :: [FloExpr] -> ([Atom], [STGBinding])
argsToAtomsBinds es = (atoms, catMaybes maybeBinds)
  where (atoms, maybeBinds) = unzip $ zipWith toAtomBind es [1..]
        toAtomBind e num
          | isAtomic e = (toAtom e, Nothing)
          | otherwise = (AtomVar var, Just $ STGBinding var $
                         LambdaForm fVars U [] (convert e))
                        where var = "__" ++ show num
                              fVars = []

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

instance Convertible [(FloExpr, FloExpr)] STGAlts where
  convert alts = STGAAlts (convert alts) --(STGDAlt2 $ STGAp "undefined" [])

{- Converts a patt -> expr pair into an STG (algebraic) alt. -}
instance Convertible (FloExpr, FloExpr) STGAAlt where
  convert (patt, expr) = STGAAlt cons vars (convert expr)
    where FloCons cons _ : exprs = flatten patt
          vars = map (\(FloVar var) -> var) exprs

-- Pretty printing
instance Pretty STGProgram where
  pp (STGProgram binds) = vcat $ map pp binds

instance Pretty STGBinding where
  pp (STGBinding n lf) = text n <+> equals <+> nest 4 (pp lf)

instance Pretty LambdaForm where
  pp LambdaForm{..} = braces (commas' $ map text fVars) <+> text "\\" <>
    pp flag <+> braces (commas' $ map text args) <+> text "->" <+> pp expr

instance Pretty UpdateFlag where
  pp U = text "u"
  pp N = text "n"

instance Pretty STGExpr where
  pp (STGLet rec binds expr) = text "let" <> rec' <+> vcat (map pp binds) $$
    text "in" <+> pp expr
    where rec' = text $ if rec then "rec" else ""
  pp (STGCase expr alts) = text "case" <+> pp expr <+> text "of" $$ pp alts
  pp (STGAp var []) = text var
  pp (STGAp var atoms) = text var <+> braces (commas' $ map pp atoms)
  pp (STGCons cons atoms) = text cons <+> braces (commas' $ map pp atoms)
  pp (STGPrim var a1 a2) = text var <+> braces (commas' [pp a1, pp a2])
  pp (STGLit lit) = int lit
  pp (STGLambda lform) = pp lform

instance Pretty STGAlts where
  pp (STGAAlts aalts {-dalt-}) = vcat (map pp aalts) -- $$ pp dalt
  pp (STGPAlts palts {-dalt-}) = vcat (map pp palts) -- $$ pp dalt

instance Pretty STGAAlt where
  pp (STGAAlt cons vars expr) = text cons <+> braces (commas' $ map text vars)
    <+> text "->" <+> pp expr

instance Pretty STGPAlt where
  pp (STGPAlt lit expr) = int lit <+> text "->" <+> pp expr

--instance Pretty STGDAlt where
--  pp (STGDAlt1 var expr) = text var <+> text "->" <+> pp expr
--  pp (STGDAlt2 expr) = text "default" <+> text "->" <+> pp expr

instance Pretty Atom where
  pp (AtomVar var) = text var
  pp (AtomLit lit) = int lit
