module HaskellProgram where

import FloGraph
import FloProgram

data HaskellExpression = HaskellLit Literal
                       | HaskellFun Name
                       | HaskellExpression :$: HaskellExpression
                       | [Input] :->: HaskellExpression
                       | HaskellLet {
                         getLetDefinitions :: [HaskellDefinition],
                         getLetExpression :: HaskellExpression
                       } deriving Show

data HaskellDefinition = HaskellDefinition {
  getDefinitionName :: Name,
  getInputs :: [Input],
  getExpression :: HaskellExpression
} deriving Show

convertFloExpression :: FloExpression -> HaskellExpression
convertFloExpression (FloLit lit) = HaskellLit lit
convertFloExpression (FloFun name _) = HaskellFun name
convertFloExpression (FloCons _ _) = undefined
convertFloExpression (FloAp expr inputs) = lambdaInputs :->: rhs
  where rhs = undefined
        lambdaInputs = undefined
convertFloExpression (FloLet ld le)
  = HaskellLet (map convertFloDefinition ld) (convertFloExpression le)

convertFloDefinition :: FloDefinition -> HaskellDefinition
convertFloDefinition fd = HaskellDefinition (FloProgram.getDefinitionName fd)
  (FloProgram.getInputs fd) (convertFloExpression $ FloProgram.getExpression fd)
