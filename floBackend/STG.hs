{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses,
    RecordWildCards #-}
module STG where

import Convertible
import FloProgram

import Data.Maybe (catMaybes)

type STGProgram = [STGBinding]

data STGBinding = STGBinding Var LambdaForm

data LambdaForm = LambdaForm {
  freeVars :: [Var],
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

data STGAlts = STGAAlts [STGAAlt] STGDAlt           -- Algebraic
             | STGPAlts [STGPAlt] STGDAlt           -- Primitive

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

instance Convertible FloProgram STGProgram where
  convert modules = convert defs
    -- Flatten the modules into a single list of declarations (for now)
    where defs = concatMap (\d -> case d of FD def -> [def]; DC _ -> [])
                 (concatMap fmDecls modules)

{- Each flo definition corresponds to an STG binding. -}
instance Convertible FloDef STGBinding where
  convert FloDef{..} = STGBinding fdName $
    LambdaForm fVars N fdInputs (convert fdExpr)
    where fVars = undefined

instance Convertible FloExpr STGExpr where
  -- Only integer literals are supported at the moment
  convert (FloLit (LitInt i)) = STGLit i
  -- Converting built-in operations consists of the following cases:
  convert (FloVar n)
    -- Case B1: Operator has no arguments
    | n `elem` primOps = STGLambda $ LambdaForm [] N [a1,a2] $
      STGPrim n (AtomVar a1) (AtomVar a2)
    | otherwise = STGAp n []
    where [a1,a2] = take 2 $ newArgs "_"
  convert (FloAp (FloVar n) e)
    -- Case B2: Operator has 1 argument
    | n `elem` primOps && length es == 1 =
      maybeLet False binds $ STGLambda $ LambdaForm [] N [arg] $
      STGPrim n (head atoms) (AtomVar arg)
    -- Case B3: Operator has 2 arguments
    | n `elem` primOps && length es == 2 =
      maybeLet False binds $ STGPrim n (head atoms) (atoms !! 1)
    where es = flatten e
          (atoms, binds) = argsToAtomsBinds es
          [arg] = take 1 $ newArgs "_"
  -- Converting constructors consists of the following cases:
  convert (FloCons n i)
    -- Case C1: Constructor takes no arguments
    | i == 0 = STGCons n []
    -- Case C2: Constructor takes arguments, but none have been applied
    | i > 0 = STGLambda $ LambdaForm [] N args expr
    where args = take i $ newArgs "_"
          expr = STGCons n (map AtomVar args)
  convert (FloAp (FloCons n i) e)
    -- Case C3: Constructor takes arguments, and all have been applied
    | i == length es = maybeLet False binds $ STGCons n atoms
    -- Case C4: Constructor takes arguments, and some have been applied
    | otherwise = maybeLet False binds $ STGLambda $ LambdaForm [] N args lexp
    where es = flatten e
          (atoms, binds) = argsToAtomsBinds es
          args = take (i - length es) $ newArgs "_"
          lexp = STGCons n $ atoms ++ map AtomVar args
  convert e@(FloAp e1 e2) = maybeLet False binds $ STGAp n atoms
    where es = flatten e
          (AtomVar n : atoms, binds) = argsToAtomsBinds es
  convert (FloLambda ns e) = STGLambda $ LambdaForm [] N ns (convert e)
  convert (FloLet defs e) = STGLet True (convert defs) (convert e)
  convert (FloCase e alts) = STGCase (convert e) (convert alts)
  convert (FloAnn _ e) = convert e

{- Flatten out a bunch of binary applications. -}
flatten :: FloExpr -> [FloExpr]
flatten (FloAp e1 e2) = e1 : flatten e2
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
                              fVars = undefined

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
  convert alts = STGAAlts (convert alts) (STGDAlt2 $ STGAp "undefined" [])

{- Converts a patt -> expr pair into an STG (algebraic) alt. -}
instance Convertible (FloExpr, FloExpr) STGAAlt where
  convert (patt, expr) = STGAAlt cons vars (convert expr)
    where FloCons cons _ : exprs = flatten patt
          vars = map (\(FloVar var) -> var) exprs
