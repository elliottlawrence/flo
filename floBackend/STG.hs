{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses,
    RecordWildCards #-}
module STG where

import Convertible
import FloProgram
import Pretty

import Control.Monad.Reader
import Control.Monad.State
import Data.List ((\\))
import qualified Data.Map as Map
import Data.Maybe (catMaybes, fromMaybe)
import qualified Data.Set as Set
import Text.PrettyPrint.Leijen hiding (Pretty)

{- An STG program consists of a list of function bindings, along with a list of
   data constructors. -}
data STGProgram = STGProgram {
  stgBindings :: [STGBinding],
  stgDataConses :: [STGDataCons]
}

data STGBinding = STGBinding Var LambdaForm

{- Since STG is untyped, the only information we need at this point about a data
   constructor is its name and arity. -}
data STGDataCons = STGDataCons Cons Int

{- A set of global or free variables -}
type Bindings = Set.Set Var

{- A map of data constructors to their arities -}
type DataConses = Map.Map Cons Int

{- A reader monad for storing the global variables and data constructors, and a
   state monad for keeping track of the current binding name -}
type RBinds a = Reader Bindings a
type RBindsDC a = Reader (Bindings, DataConses) a
type StRBindsDC a = StateT Var (Reader (Bindings, DataConses)) a

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

data STGAlts = STGAAlts [STGAAlt] (Maybe STGDAlt)   -- Algebraic
             | STGPAlts [STGPAlt] (Maybe STGDAlt)   -- Primitive

data STGAAlt = STGAAlt Cons [Var] STGExpr           -- Algebraic alt
data STGPAlt = STGPAlt Lit STGExpr                  -- Primitive alt
data STGDAlt = STGDAlt (Maybe Var) STGExpr          -- Default alt

{- So far, only integer literals are supported. -}
type Lit = Int

data Atom = AtomVar Var | AtomLit Lit

type Var = String
type Cons = String

{- The primitive binary operations. -}
primOps :: [String]
primOps = ["+#", "-#", "*#", "/#"]

{- Find the arity of the given data constructor -}
getDataConsArity :: DataConses -> Cons -> Int
getDataConsArity dataConses name =
  fromMaybe (error "Data constructor not found") $ Map.lookup name dataConses

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
  free (STGDAlt (Just var) e) = liftM (Set.delete var) (free e)
  free (STGDAlt Nothing e) = free e

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
  convert FloProgram{..} = STGProgram
    (runReader (mapM convert fpDefs) (globals,dataConsMap)) dataConses
    where dataConses = map
            (\dc -> STGDataCons (dcName dc) (length $ dcFields dc)) fpDataConses
          dataConsMap = foldl (\map (STGDataCons name arity) ->
            Map.insert name arity map) Map.empty dataConses
          -- The globals are the top level definitions
          globals = Set.fromList $ map fdName fpDefs

{- Each flo definition corresponds to an STG binding. -}
instance Convertible FloDef (RBindsDC STGBinding) where
  convert FloDef{..} = do
    (globs,_) <- ask
    expr <- evalStateT (convert fdExpr) fdName
    let lForm = runReader (createLForm fdInputs expr) globs
    return $ STGBinding fdName lForm

instance Convertible FloExpr (StRBindsDC STGExpr) where
  -- Only integer literals are supported at the moment
  convert (FloLit (LitInt i)) = return $ STGLit i

  convert (FloVar n)
    -- Case B1: Operator has no arguments
    | n `elem` primOps = do
        (globs,_) <- ask
        let [a1,a2] = take 2 $ newArgs "_"
            lForm = runReader (createLForm [a1,a2] $
                    STGPrim n (AtomVar a1) (AtomVar a2)) globs
        stgLambda lForm
    | otherwise = return $ STGAp n []

  convert (FloCons n) = do
    (globs,dataConses) <- ask
    let i = getDataConsArity dataConses n
        args = take i $ newArgs "_"
        expr = STGCons n (map AtomVar args)
        lForm = runReader (createLForm args expr) globs
    -- Case C1: Constructor takes no arguments
    if i == 0 then return $ STGCons n []
    -- Case C2: Constructor takes arguments, but none have been applied
    else stgLambda lForm

  convert aps@(FloAp e1 e2) = do
    (globs,dataConses) <- ask
    let e:es = flatten aps
        retLetF letb lete = return $ maybeLet False letb lete
    (AtomVar av : atoms, binds) <- argsToAtomsBinds (e:es)

    case e of
    -- Note: If e is a constructor, argsToAtomsBinds will create a binding
    -- for it, so we must ignore it.
      FloCons n
        -- Case C3: Constructor takes arguments, and all have been applied
        | i == length es -> retLetF (tail binds) (STGCons n atoms)
        -- Case C4: Constructor takes arguments, and some have been
        -- applied
        | otherwise -> do
          let args = take (i - length es) $ newArgs "_"
              lForm = runReader (createLForm args $
                      STGCons n $ atoms ++ map AtomVar args) globs
          e <- stgLambda lForm
          retLetF (tail binds) e
        where i = getDataConsArity dataConses n
      FloVar n
        -- Case B2: Operator has 1 argument
        | n `elem` primOps && length es == 1 -> do
          let lForm = runReader (createLForm [arg] lexp) globs
          e <- stgLambda lForm
          retLetF binds e
        -- Case B3: Operator has 2 arguments
        | n `elem` primOps && length es == 2 ->
          retLetF binds (STGPrim n (head atoms) (atoms !! 1))
        where [arg] = take 1 $ newArgs "_"
              lexp = STGPrim n (head atoms) (AtomVar arg)
      -- Default case
      otherwise -> retLetF binds (STGAp av atoms)

  convert (FloLambda ns e) = do
    (globs,_) <- ask
    e' <- convert e
    let lForm = runReader (createLForm ns e') globs
    stgLambda lForm

  convert (FloLet defs e) = do
    reader <- ask
    let defs' = runReader (mapM convert defs) reader
    liftM (STGLet True defs') (convert e)

  convert (FloCase e alts) = liftM2 STGCase (convert e) (convert alts)

  convert (FloAnn _ e) = convert e

{- When saturating constructors and primitives, extra lambdas need to be added.
   Since lambda forms are not expressions, they need to be wrapped in a let. -}
stgLambda :: LambdaForm -> StRBindsDC STGExpr
stgLambda lForm = do
  name <- get
  let newBind = name ++ head (take 1 (newArgs "_"))
  return $ STGLet False [STGBinding newBind lForm] (STGAp newBind [])

{- Flatten out a bunch of binary applications. -}
flatten :: FloExpr -> [FloExpr]
flatten (FloAp e1 e2) = flatten e1 ++ [e2]
flatten e = [e]

{- An infinite list of arguments with the given prefix -}
newArgs :: String -> [String]
newArgs prefix = map (\num -> prefix ++ show num) [1..]

{- Converts a list of expressions (function arguments) to a list of atomic
   expressions and bindings -}
argsToAtomsBinds :: [FloExpr] -> StRBindsDC ([Atom], [STGBinding])
argsToAtomsBinds es = do
  (atoms, maybeBinds) <- mapAndUnzipM toAtomBind (zip es [0..])
  return (atoms, catMaybes maybeBinds)
  where
    toAtomBind :: (FloExpr, Int) -> StRBindsDC (Atom, Maybe STGBinding)
    toAtomBind (e,num)
      | isAtomic e = return (toAtom e, Nothing)
      | otherwise = do
        (globs,_) <- ask
        -- Update the name of the binding and then reset it
        name <- get
        let var = name ++ "_" ++ show num
        put var
        e' <- convert e
        put name
        -- Create the binding
        let lForm = runReader (createLForm [] e') globs
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

instance Convertible [(FloExpr, FloExpr)] (StRBindsDC STGAlts) where
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
instance Convertible (FloExpr, FloExpr) (StRBindsDC Alt) where
  convert (patt, expr) = liftM
    (case flatten patt of
      FloCons cons : exprs -> AAlt . STGAAlt cons vars
        where vars = map (\(FloVar var) -> var) exprs
      [FloLit (LitInt i)] -> PAlt . STGPAlt i
      [FloVar var] -> DAlt . STGDAlt (Just var)
      _ -> DAlt . STGDAlt Nothing) (convert expr)

-- Pretty printing
instance Pretty STGProgram where
  pp STGProgram{..} = {-pp stgDataConses <$$>-} pp stgBindings

instance Pretty STGDataCons where
  pp (STGDataCons name arity) = text name <+> int arity

instance Pretty STGBinding where
  pp (STGBinding n lf) = text n <+> equals <+> nest 4 (pp lf)

instance Pretty LambdaForm where
  pp LambdaForm{..} = braces (commas' $ map text (Set.toList fVars)) <+>
    text "\\" <> pp flag <+> align (braces (commas' $ map text args) <+>
    text "->" </> align (pp expr))

instance Pretty UpdateFlag where
  pp U = char 'u'
  pp N = char 'n'

instance Pretty STGExpr where
  pp (STGLet rec binds expr) = text "let" <> rec' <+> align (pp binds) <$$>
    text "in" <+> align (pp expr)
    where rec' = text $ if rec then "rec" else ""
  pp (STGCase expr alts) = text "case" <+> pp expr <+> text "of" <$$>
    indent 2 (pp alts)
  pp (STGAp var []) = text var
  pp (STGAp var atoms) = text var <+> braces (commas' $ map pp atoms)
  pp (STGCons cons atoms) = text cons <+> braces (commas' $ map pp atoms)
  pp (STGPrim var a1 a2) = text var <+> braces (commas' [pp a1, pp a2])
  pp (STGLit lit) = int lit

instance Pretty STGAlts where
  pp (STGAAlts aalts dalt) = pp aalts <$$> pp dalt
  pp (STGPAlts palts dalt) = pp palts <$$> pp dalt

instance Pretty STGAAlt where
  pp (STGAAlt cons vars expr) = text cons <+> braces (commas' $ map text vars)
    <+> text "->" <+> align (pp expr)

instance Pretty STGPAlt where
  pp (STGPAlt lit expr) = int lit <+> text "->" <+> align (pp expr)

instance Pretty STGDAlt where
  pp (STGDAlt (Just var) expr) = text var <+> text "->" <+> align (pp expr)
  pp (STGDAlt Nothing expr) = text "default" <+> text "->" <+> align (pp expr)

instance Pretty Atom where
  pp (AtomVar var) = text var
  pp (AtomLit lit) = int lit
