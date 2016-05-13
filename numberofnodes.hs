module NumberOfNodes where
import QcSyntax
import qualified Data.List as List

numberOfNodes :: Program -> Int
numberOfNodes p = sum (List.map numberOfNodesToplevel p)

numberOfNodesToplevel :: ToplevelForm -> Int
numberOfNodesToplevel (ToplevelDefinition v e) =
  2 + numberOfNodesVar v + numberOfNodesExpression e
numberOfNodesToplevel (ToplevelExpression e) =
  numberOfNodesExpression e

numberOfNodesVar :: Variable -> Int
numberOfNodesVar _ = 1

genericNumberOfNodes :: (a -> Int) -> [a] -> Int
genericNumberOfNodes f = sum . List.map f

numberOfNodesVars :: [Variable] -> Int
numberOfNodesVars = genericNumberOfNodes numberOfNodesVar

numberOfNodesExpressions :: [Expression] -> Int
numberOfNodesExpressions = genericNumberOfNodes numberOfNodesExpression

numberOfNodesCondClause :: CondClause -> Int
numberOfNodesCondClause (Immediate e) =
  sum [1, numberOfNodesExpression e]
numberOfNodesCondClause (Guarded e1 e2) =
  sum [2, numberOfNodesExpressions [e1, e2]]
numberOfNodesCondClause (GuardedImpl e1 e2) =
  sum [2, numberOfNodesExpressions [e1, e2]]

numberOfNodesQuotation :: Quotation -> Int
numberOfNodesQuotation (QuoteNumber _) = 1
numberOfNodesQuotation (QuoteBoolean _) = 1
numberOfNodesQuotation (QuoteChar _) = 1
numberOfNodesQuotation (QuoteString _) = 1
numberOfNodesQuotation (QuoteSymbol _) = 1
numberOfNodesQuotation QuoteEmpty = 1
numberOfNodesQuotation (QuotePair q1 q2) =
  sum [2, numberOfNodesQuotation q1, numberOfNodesQuotation q2]

numberOfNodesCase :: CaseClause -> Int
numberOfNodesCase (CaseClause qs e) =
  sum [1, genericNumberOfNodes numberOfNodesQuotation qs,
       numberOfNodesExpression e]

numberOfNodesBindings :: [LetBinding] -> Int
numberOfNodesBindings = genericNumberOfNodes numberOfNodesBinding

numberOfNodesBinding :: LetBinding -> Int
numberOfNodesBinding (LetBinding v e) =
  sum [2, numberOfNodesVar v, numberOfNodesExpression e]

numberOfNodesRecBinding :: LetrecBinding -> Int
numberOfNodesRecBinding (LetrecBinding v lambda) =
  sum [2, numberOfNodesVar v, numberOfNodesLambda lambda]

numberOfNodesLambda :: LambdaAbstraction -> Int
numberOfNodesLambda (Lambda vs e) =
  sum [1, numberOfNodesVars vs, numberOfNodesExpression e]
numberOfNodesLambda (TraceLambda v vs e) =
  sum [2, numberOfNodesVars (v:vs), numberOfNodesExpression e]

numberOfNodesExpression :: Expression -> Int
numberOfNodesExpression (Number _) = 1
numberOfNodesExpression (Boolean _) = 1
numberOfNodesExpression (Ch _) = 1
numberOfNodesExpression (Str _) = 1
numberOfNodesExpression (Time e) = sum [1, numberOfNodesExpression e]
numberOfNodesExpression (If e1 e2 e3) =
  sum [3, numberOfNodesExpressions [e1, e2, e3]]
numberOfNodesExpression (And es) = numberOfNodesExpressions es
numberOfNodesExpression (Or es) = numberOfNodesExpressions es
numberOfNodesExpression (Cond clauses e) =
  sum [2, genericNumberOfNodes numberOfNodesCondClause clauses,
       numberOfNodesExpression e]
numberOfNodesExpression (Case e1 clauses e2) =
  sum [3, genericNumberOfNodes numberOfNodesCase clauses,
       numberOfNodesExpressions [e1, e2]]
numberOfNodesExpression (Let bindings e) =
  sum [1, numberOfNodesBindings bindings, numberOfNodesExpression e]
numberOfNodesExpression (Letstar bindings e) =
  sum [2, numberOfNodesBindings bindings, numberOfNodesExpression e]
numberOfNodesExpression (Letrec bindings e) =
  sum [1, genericNumberOfNodes numberOfNodesRecBinding bindings,
       numberOfNodesExpression e]
numberOfNodesExpression (Begin e1 e2) =
  sum [2, numberOfNodesExpressions [e1, e2]]
numberOfNodesExpression (Unless e1 e2) =
  sum [2, numberOfNodesExpressions [e1, e2]]
numberOfNodesExpression (Quote q) =
  sum [1, numberOfNodesQuotation q]
numberOfNodesExpression (LambdaExpression lambda) =
  numberOfNodesLambda lambda
numberOfNodesExpression (Application e1 e2) =
  sum [2, numberOfNodesExpressions [e1, e2]]
