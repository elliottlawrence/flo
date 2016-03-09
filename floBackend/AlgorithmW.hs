{- Taken (and adapted) with permission from "Algorithm W Step by Step" by Martin
   Grabmuller -}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module AlgorithmW where

import FloProgram (FloExpr, Literal(..))
import Pretty

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import Text.PrettyPrint

data Exp = EVar String                -- Variables
         | ELit Literal               -- Literals
         | EApp Exp Exp               -- Applications
         | EAbs String Exp            -- Lambda abstractions
         | ELet String Exp Exp        -- Let expressions
         | ELetRec [(String,Exp)] Exp -- Recursive let expressions

data Type = TVar String               -- Type variable
          | TCons String [Type]       -- Type constructors
          deriving Eq

{- A function type is a type constructor of arity two. -}
tFun :: Type -> Type -> Type
tFun t1 t2 = TCons "->" [t1, t2]

{- A literal is essentially a type constructor of arity zero. -}
tLit :: Literal -> Type
tLit l = TCons (case l of LitInt _ -> "Int"
                          LitFloat _ -> "Float"
                          LitChar _ -> "Char"
                          LitString _ -> "String") []

{- A type scheme is a type which contains one or more variables bound by forall
   quantifiers. -}
data Scheme = Scheme [String] Type

class Types a where
  -- Determines the free variables in a type
  ftv :: a -> Set.Set String
  -- Applies a substition to a type
  apply :: Subst -> a -> a

instance Types Type where
  ftv (TVar n) = Set.singleton n
  ftv (TCons n args) = foldr (Set.union . ftv) Set.empty args

  apply s (TVar n) = fromMaybe (TVar n) (Map.lookup n s)
  apply s (TCons n args) = TCons n $ map (apply s) args

instance Types Scheme where
  ftv (Scheme vars t) = ftv t Set.\\ Set.fromList vars
  apply s (Scheme vars t) = Scheme vars $ apply (foldr Map.delete s vars) t

instance Types a => Types [a] where
  apply s = map $ apply s
  ftv = foldr (Set.union . ftv) Set.empty

{- A substition is a mapping from type variables to types. -}
type Subst = Map.Map String Type

nullSubst :: Subst
nullSubst = Map.empty

{- Performs the substitution s2 followed by s1. -}
composeSubst :: Subst -> Subst -> Subst
s1 `composeSubst` s2 = Map.map (apply s1) s2 `Map.union` s1

{- Type environments are mappings from term variables to their respective type
   schemes. -}
newtype TypeEnv = TypeEnv (Map.Map String Scheme)

remove :: TypeEnv -> String -> TypeEnv
remove (TypeEnv env) var = TypeEnv $ Map.delete var env

instance Types TypeEnv where
  ftv (TypeEnv env) = ftv $ Map.elems env
  apply s (TypeEnv env) = TypeEnv $ Map.map (apply s) env

{- Generalize abstracts a type over all type variables which are free in the
   type but not free in the given type environment. -}
generalize :: TypeEnv -> Type -> Scheme
generalize env t = Scheme vars t
  where vars = Set.toList $ ftv t Set.\\ ftv env

{- The state of the type inference consists of a name supply and a set of
   substitutions. -}
data TIState = TIState {
  tiSupply :: Int,
  tiSubst :: Subst
}

{- The type inference monad is capable of generating free names for newly
   introduced type variables and handling exceptions. -}
type TI a = ExceptT String (StateT TIState IO) a

runTI :: TI a -> IO (Either String a, TIState)
runTI t = runStateT (runExceptT t) initTIState
  where initTIState = TIState {tiSupply = 0, tiSubst = Map.empty}

newTyVar :: TI Type
newTyVar = do s <- get
              put s {tiSupply = tiSupply s + 1}
              return $ TVar $ "a" ++ show (tiSupply s)

instantiate :: Scheme -> TI Type
instantiate (Scheme vars t) = do nvars <- mapM (const newTyVar) vars
                                 let s = Map.fromList $ zip vars nvars
                                 return $ apply s t

-- Unification
unify :: Type -> Type -> TI Subst
unify (TCons n args) (TCons n' args') | n == n' = unifyl args args'
unify (TVar u) t = varBind u t
unify t (TVar u) = varBind u t
unify t1 t2 = throwError $ "types do not unify: " ++ showP t1 ++ " vs. " ++
  showP t2

unifyl :: [Type] -> [Type] -> TI Subst
unifyl [] [] = return nullSubst
unifyl (t:ts) (t':ts') = do s1 <- unify t t'
                            s2 <- unifyl (apply s1 ts) (apply s1 ts')
                            return $ s1 `composeSubst` s2

varBind :: String -> Type -> TI Subst
varBind u t | t == TVar u = return nullSubst
            | u `Set.member` ftv t = throwError $ "occur check fails: " ++ u ++
              " vs. " ++ showP t
            | otherwise = return $ Map.singleton u t

-- Type inference
ti :: TypeEnv -> Exp -> TI (Subst, Type)
ti (TypeEnv env) (EVar n) = case Map.lookup n env of
                              Nothing -> throwError $ "unbound variable: " ++ n
                              Just sigma -> do t <- instantiate sigma
                                               return (nullSubst, t)
ti env (ELit l) = return (nullSubst, tLit l)
ti env (EAbs n e) = do
  tv <- newTyVar
  let TypeEnv env' = remove env n
      env'' = TypeEnv $ env' `Map.union` Map.singleton n (Scheme [] tv)
  (s1, t1) <- ti env'' e
  return (s1, tFun (apply s1 tv) t1)
ti env (EApp e1 e2) = do
  tv <- newTyVar
  (s1, t1) <- ti env e1
  (s2, t2) <- ti (apply s1 env) e2
  s3 <- unify (apply s2 t1) (tFun t2 tv)
  return (s3 `composeSubst` s2 `composeSubst` s1, apply s3 tv)
ti env (ELet x e1 e2) = do
  (s1, t1) <- ti env e1
  let TypeEnv env' = remove env x
      t' = generalize (apply s1 env) t1
      env'' = TypeEnv $ Map.insert x t' env'
  (s2, t2) <- ti (apply s1 env'') e2
  return (s1 `composeSubst` s2, t2)

typeInference :: Exp -> TI Type
typeInference e = do (s, t) <- ti (TypeEnv Map.empty) e
                     return $ apply s t

-- Pretty printing functions
instance Pretty Type where
  pp (TVar n) = text n
  pp (TCons "->" [t,s]) = prParenType t <+> text "->" <+> pp s
    where prParenType t = case t of
                            TCons _ (_:_) -> parens (pp t)
                            _ -> pp t
  pp (TCons n args) = text n <+> hsep (map pp args)

instance Pretty Exp where
  pp (EVar name) = text name
  pp (ELit lit) = pp lit
  pp (ELet x b body) = text "let" <+> text x <+> text "=" <+> pp b <+>
    text "in" $$ nest 2 (pp body)
  pp (ELetRec binds body) = text "letrec" <+>
    vcat (map (\(x,e) -> text x <+> text "=" <+> pp e) binds) <+> text "in" $$
    nest 2 (pp body)
  pp (EAbs n e) = char '\\' <+> text n <+> text "->" <+> pp e
  pp (EApp e1 e2)  = pp e1 <+> prParenExp e2
    where prParenExp t = case t of
                          ELet{} -> parens (pp t)
                          ELetRec{} -> parens (pp t)
                          EApp{} -> parens (pp t)
                          EAbs{} -> parens (pp t)
                          _ -> pp t

instance Pretty Scheme where
  pp (Scheme vars t) = text "All" <+> hcat (punctuate comma $ map text vars)
    <> text "." <+> pp t

-- Tests
e0 = ELet "id" (EAbs "x" (EVar "x"))
      (EVar "id")
e1 = ELet "id" (EAbs "x" (EVar "x"))
      (EApp (EVar "id") (EVar "id"))
e2 = ELet "id" (EAbs "x" (ELet "y" (EVar "x") (EVar "y")))
      (EApp (EVar "id") (EVar "id"))
e3 = ELet "id" (EAbs "x" (ELet "y" (EVar "x") (EVar "y")))
      (EApp (EApp (EVar "id") (EVar "id")) (ELit (LitInt 2)))
e4 = ELet "id" (EAbs "x" (EApp (EVar "x") (EVar "x")))
      (EVar "id")
e5 = EAbs "m" (ELet "y" (EVar "m")
      (ELet "x" (EApp (EVar "y") (ELit (LitChar 'c')))
        (EVar "x")))

test :: Exp -> IO ()
test e = do (res, _) <- runTI $ typeInference e
            case res of
              Left err -> putStrLn $ "error: " ++ err
              Right t -> putStrLn $ showP e ++ " :: " ++ showP t

main :: IO ()
main = mapM_ test [e0, e1, e2, e3, e4, e5]
