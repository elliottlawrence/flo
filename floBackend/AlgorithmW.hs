-- Taken with permission from "Algorithm W Step by Step" by Martin Grabmuller
module AlgorithmW where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import Text.PrettyPrint

data Exp = EVar String
         | ELit Lit
         | EApp Exp Exp
         | EAbs String Exp
         | ELet String Exp Exp

data Lit = LInt Integer
         | LBool Bool

data Type = TVar String
          | TInt
          | TBool
          | TFun Type Type
          deriving Eq

data Scheme = Scheme [String] Type

class Types a where
  ftv :: a -> Set.Set String
  apply :: Subst -> a -> a

instance Types Type where
  ftv (TVar n) = Set.singleton n
  ftv TInt = Set.empty
  ftv TBool = Set.empty
  ftv (TFun t1 t2) = ftv t1 `Set.union` ftv t2

  apply s (TVar n) = fromMaybe (TVar n) (Map.lookup n s)
  apply s (TFun t1 t2) = TFun (apply s t1) (apply s t2)
  apply s t = t

instance Types Scheme where
  ftv (Scheme vars t) = ftv t Set.\\ Set.fromList vars
  apply s (Scheme vars t) = Scheme vars $ apply (foldr Map.delete s vars) t

instance Types a => Types [a] where
  apply s = map (apply s)
  ftv = foldr (Set.union . ftv) Set.empty

type Subst = Map.Map String Type

nullSubst :: Subst
nullSubst = Map.empty

composeSubst :: Subst -> Subst -> Subst
composeSubst s1 s2 = Map.map (apply s1) s2 `Map.union` s1

newtype TypeEnv = TypeEnv (Map.Map String Scheme)

remove :: TypeEnv -> String -> TypeEnv
remove (TypeEnv env) var = TypeEnv $ Map.delete var env

instance Types TypeEnv where
  ftv (TypeEnv env) = ftv $ Map.elems env
  apply s (TypeEnv env) = TypeEnv $ Map.map (apply s) env

generalize :: TypeEnv -> Type -> Scheme
generalize env t = Scheme vars t
  where vars = Set.toList $ ftv t Set.\\ ftv env

data TIEnv = TIEnv

data TIState = TIState {
  tiSupply :: Int,
  tiSubst :: Subst
}

type TI a = ExceptT String (ReaderT TIEnv (StateT TIState IO)) a

runTI :: TI a -> IO (Either String a, TIState)
runTI t = runStateT (runReaderT (runExceptT t) initTIEnv) initTIState
  where initTIEnv = TIEnv
        initTIState = TIState {tiSupply = 0, tiSubst = Map.empty}

newTyVar :: String -> TI Type
newTyVar prefix = do s <- get
                     put s {tiSupply = tiSupply s + 1}
                     return $ TVar (prefix ++ show (tiSupply s))

instantiate :: Scheme -> TI Type
instantiate (Scheme vars t) = do nvars <- mapM (\_ -> newTyVar "a") vars
                                 let s = Map.fromList $ zip vars nvars
                                 return $ apply s t

-- Unification
mgu :: Type -> Type -> TI Subst
mgu (TFun l r) (TFun l' r') = do s1 <- mgu l l'
                                 s2 <- mgu (apply s1 r) (apply s1 r')
                                 return $ s1 `composeSubst` s2
mgu (TVar u) t = varBind u t
mgu t (TVar u) = varBind u t
mgu TInt TInt = return nullSubst
mgu TBool TBool = return nullSubst
mgu t1 t2 = throwError $ "types do not unify: " ++ show t1 ++ " vs. " ++ show t2

varBind :: String -> Type -> TI Subst
varBind u t | t == TVar u = return nullSubst
            | u `Set.member` ftv t = throwError $ "occur check fails: " ++ u ++
              " vs. " ++ show t
            | otherwise = return $ Map.singleton u t

-- Type inference
tiLit :: TypeEnv -> Lit -> TI (Subst, Type)
tiLit _ (LInt _) = return (nullSubst, TInt)
tiLit _ (LBool _) = return (nullSubst, TBool)

ti :: TypeEnv -> Exp -> TI (Subst, Type)
ti (TypeEnv env) (EVar n) = case Map.lookup n env of
                              Nothing -> throwError $ "unbound variable: " ++ n
                              Just sigma -> do t <- instantiate sigma
                                               return (nullSubst, t)
ti env (ELit l) = tiLit env l
ti env (EAbs n e) = do
  tv <- newTyVar "a"
  let TypeEnv env' = remove env n
      env'' = TypeEnv $ env' `Map.union` Map.singleton n (Scheme [] tv)
  (s1, t1) <- ti env'' e
  return (s1, TFun (apply s1 tv) t1)
ti env (EApp e1 e2) = do
  tv <- newTyVar "a"
  (s1, t1) <- ti env e1
  (s2, t2) <- ti (apply s1 env) e2
  s3 <- mgu (apply s2 t1) (TFun t2 tv)
  return (s3 `composeSubst` s2 `composeSubst` s1, apply s3 tv)
ti env (ELet x e1 e2) = do
  (s1, t1) <- ti env e1
  let TypeEnv env' = remove env x
      t' = generalize (apply s1 env) t1
      env'' = TypeEnv $ Map.insert x t' env'
  (s2, t2) <- ti (apply s1 env'') e2
  return (s1 `composeSubst` s2, t2)

typeInference :: Map.Map String Scheme -> Exp -> TI Type
typeInference env e = do (s, t) <- ti (TypeEnv env) e
                         return $ apply s t

-- Pretty printing functions
instance Show Type where
  showsPrec _ x = shows (prType x)

prType :: Type -> Doc
prType (TVar n) = text n
prType TInt = text "Int"
prType TBool = text "Bool"
prType (TFun t s) = prParenType t <+> text "->" <+> prType s

prParenType :: Type -> Doc
prParenType t = case t of
                  TFun _ _ -> parens (prType t)
                  _ -> prType t

instance Show Exp where
  showsPrec _ x = shows (prExp x)

prExp :: Exp -> Doc
prExp (EVar name) = text name
prExp (ELit lit) = prLit lit
prExp (ELet x b body) = text "let" <+> text x <+> text "=" <+> prExp b <+>
  text "in" $$ nest 2 (prExp body)
prExp (EApp e1 e2)  = prExp e1 <+> prParenExp e2
prExp (EAbs n e) = char '\\' <+> text n <+> text "->" <+> prExp e

prParenExp :: Exp -> Doc
prParenExp t = case t of
                ELet{} -> parens (prExp t)
                EApp{} -> parens (prExp t)
                EAbs{} -> parens (prExp t)
                _ -> prExp t

instance Show Lit where
  showsPrec _ x = shows (prLit x)

prLit :: Lit -> Doc
prLit (LInt i) = integer i
prLit (LBool b) = if b then text "True" else text "False"

instance Show Scheme where
  showsPrec _ x = shows (prScheme x)

prScheme :: Scheme -> Doc
prScheme (Scheme vars t) = text "All" <+> hcat (punctuate comma $ map text vars)
  <> text "." <+> prType t

-- Tests
e0 = ELet "id" (EAbs "x" (EVar "x"))
      (EVar "id")
e1 = ELet "id" (EAbs "x" (EVar "x"))
      (EApp (EVar "id") (EVar "id"))
e2 = ELet "id" (EAbs "x" (ELet "y" (EVar "x") (EVar "y")))
      (EApp (EVar "id") (EVar "id"))
e3 = ELet "id" (EAbs "x" (ELet "y" (EVar "x") (EVar "y")))
      (EApp (EApp (EVar "id") (EVar "id")) (ELit (LInt 2)))
e4 = ELet "id" (EAbs "x" (EApp (EVar "x") (EVar "x")))
      (EVar "id")
e5 = EAbs "m" (ELet "y" (EVar "m")
      (ELet "x" (EApp (EVar "y") (ELit (LBool True)))
        (EVar "x")))

test :: Exp -> IO ()
test e = do (res, _) <- runTI $ typeInference Map.empty e
            case res of
              Left err -> putStrLn $ "error: " ++ err
              Right t -> putStrLn $ show e ++ " :: " ++ show t

main :: IO ()
main = mapM_ test [e0, e1, e2, e3, e4, e5]
