module HaskellProgram where

import FloGraph
import FloProgram

import Data.List (intercalate)

data HaskellExpression = HaskellLit Literal
                       | HaskellFun Name
                       | HaskellExpression :$: HaskellExpression
                       | [Input] :->: HaskellExpression
                       | HaskellLet {
                         getLetDefinitions :: [HaskellDefinition],
                         getLetExpression :: HaskellExpression
                       }

showInputs :: [Input] -> String
showInputs inputs = unwords $ map (fixName . getInputName) inputs

instance Show HaskellExpression where
  show (HaskellLit lit) = show lit
  show (HaskellFun fun) = fixName fun
  show (e1 :$: e2) = show e1 ++ " " ++ e2'
    where e2' | isAtomic e2 = show e2
              | otherwise = "(" ++ show e2 ++ ")"
  show (inputs :->: expr)
    = '\\' : showInputs inputs ++ " -> " ++ show expr
  show (HaskellLet ld le)
    = "let {" ++ intercalate "; " (map show ld) ++ "} in " ++ show le

isAtomic :: HaskellExpression -> Bool
isAtomic (HaskellLit _) = True
isAtomic (HaskellFun _) = True
isAtomic _ = False

data HaskellDefinition = HaskellDefinition {
  getDefinitionName :: Name,
  getDefinitionInputs :: [Input],
  getExpression :: HaskellExpression
}

instance Show HaskellDefinition where
  show (HaskellDefinition name inputs expr)
    = fixName name ++ space ++ showInputs inputs ++ " = " ++ show expr
      where space | null inputs = ""
                  | otherwise = " "

data HaskellModule = HaskellModule {
  getHaskellModuleName :: Name,
  getHaskellModuleDefinitions :: [HaskellDefinition]
}

instance Show HaskellModule where
  show (HaskellModule name defs) = "module " ++ name ++ " where\n\n" ++
    intercalate "\n\n" (map show defs)

data HaskellProgram = HaskellProgram [HaskellModule]

convertFloProgram :: FloProgram -> HaskellProgram
convertFloProgram (FloProgram modules)
  = HaskellProgram $ map convertFloModule modules

convertFloModule :: FloModule -> HaskellModule
convertFloModule m = HaskellModule (getFloModuleName m)
  (map convertFloDefinition $ getFloModuleDefinitions m)

convertFloDefinition :: FloDefinition -> HaskellDefinition
convertFloDefinition fd = HaskellDefinition
  (FloProgram.getDefinitionName fd)
  (FloProgram.getDefinitionInputs fd)
  (convertFloExpression $ FloProgram.getExpression fd)

convertFloExpression :: FloExpression -> HaskellExpression
convertFloExpression (FloLit lit) = HaskellLit lit
convertFloExpression (FloFun name _) = HaskellFun $ fixName name
convertFloExpression (FloCons _ _) = undefined
convertFloExpression e@(FloAp expr apps)
  | null lambdaInputs = rhs
  | otherwise = lambdaInputs :->: rhs
  where rhs = foldl1 (:$:) $ convertFloExpression expr : exprApps
        lambdaInputs = getInputs e
        exprInputs = getInputs expr
        exprApps = makeApps exprInputs apps
convertFloExpression (FloLet ld le)
  = HaskellLet (map convertFloDefinition ld) (convertFloExpression le)

makeApps :: [Input] -> [(Input, FloExpression)] -> [HaskellExpression]
makeApps inputs [] = map (HaskellFun . getInputName) inputs
makeApps [] apps = []
makeApps (i:inputs) a@((i',e):apps)
  | i == i' = convertFloExpression e : makeApps inputs apps
  | otherwise = HaskellFun (getInputName i) : makeApps inputs a

{- To avoid name clashes with existing things in Haskell -}
fixName :: String -> String
fixName "if" = "iff"
fixName "then" = "thenn"
fixName "else" = "elsee"
fixName name = name
