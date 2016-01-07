{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses #-}
module STG where

import Convertible
import FloProgram

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
             | STGApp Var [Atom]                    -- Application
             | STGConstr Constr [Atom]              -- Constructor
             | STGPrim Prim [Atom]                  -- Built-in operator
             | STGLit Lit

data STGAlts = STGAAlts [STGAAlt] STGDAlt
             | STGPAlts [STGPAlt] STGDAlt

data STGAAlt = STGAAlt Constr [Var] STGExpr         -- Algebraic alt
data STGPAlt = STGPAlt Lit STGExpr                  -- Primitive alt
data STGDAlt = STGDAlt1 Var STGExpr                 -- Default alt
             | STGDAlt2 STGExpr

{- So far, only integer literals are supported. -}
type Lit = Int

{- The primitive integer ops are binary operations. -}
data Prim = Prim (Int -> Int -> Int)

data Atom = AtomVar Var | AtomLit Lit

type Var = String
type Constr = String

instance Convertible FloProgram STGProgram where
  convert (FloProgram modules) = undefined
