module Unparse where
import QcSyntax

unparse :: Program -> String
unparse = concatMap unparseToplevel

unparseToplevel :: ToplevelForm -> String
unparseToplevel (ToplevelDefinition var e) =
  "(define " ++ unparseVar var ++ " " ++ unparseExpr e ++ ")\n"
unparseToplevel (ToplevelExpression e) = unparseExpr e ++ "\n"

unparseVar :: Variable -> String
unparseVar (Variable v) = v

unparseExpr :: Expression -> String
unparseExpr (Number n) = show n
unparseExpr (Boolean True) = "#t"
unparseExpr (Boolean False) = "#f"
unparseExpr (Ch ch) = "#\\" ++ [ch]
unparseExpr (Str s) = show s
unparseExpr (Time e) = "(time " ++ unparseExpr e ++ ")"
unparseExpr (If e1 e2 e3) =
    "(if " ++ unparseExpr e1 ++
    " " ++ unparseExpr e2 ++
    " " ++ unparseExpr e3 ++ ")"
unparseExpr (And es) = "(and " ++ unwords (map unparseExpr es) ++ ")"
unparseExpr (Or es) = "(or " ++ unwords (map unparseExpr es) ++ ")"
unparseExpr (Cond clauses e) =
  "(cond " ++ unwords (map unparseCondClause clauses) ++
  " [else " ++ unparseExpr e ++ "])"
unparseExpr (Case e1 clauses e2) =
  "(case " ++ unparseExpr e1 ++
  " " ++ unwords (map unparseCaseClause clauses) ++
  " [else " ++ unparseExpr e2 ++ "])"
unparseExpr (Let bindings e) =
  "(let (" ++ unwords (map unparseLetbinding bindings) ++
  ") " ++ unparseExpr e ++ ")"
unparseExpr (Letstar bindings e) =
  "(let* (" ++ unwords (map unparseLetbinding bindings) ++
  ") " ++ unparseExpr e ++ ")"
unparseExpr (Letrec bindings e) =
  "(letrec (" ++ unwords (map unparseLetrecBinding bindings) ++
  ") " ++ unparseExpr e ++ ")"
unparseExpr (Begin e1 e2) =
  "(begin " ++ unparseExpr e1 ++ " " ++ unparseExpr e2 ++ ")"
unparseExpr (Unless e1 e2) =
  "(unless " ++ unparseExpr e1 ++ " " ++ unparseExpr e2 ++ ")"
unparseExpr (Quote q) = "(quote " ++ unparseQuotation q ++ ")"
unparseExpr (LambdaExpression lambda) = unparseLambdaAbstraction lambda
unparseExpr (Application e1 e2) =
  "(" ++ unparseExpr e1 ++ " " ++ unparseExpr e2 ++ ")"

unparseCondClause :: CondClause -> String
unparseCondClause (Immediate e) = "[" ++ unparseExpr e ++ "]"
unparseCondClause (Guarded e1 e2) =
  "[" ++ unparseExpr e1 ++
  " " ++ unparseExpr e2 ++ "]"
unparseCondClause (GuardedImpl e1 e2) =
    "[" ++ unparseExpr e1 ++
    " => " ++ unparseExpr e2 ++ "]"

unparseCaseClause :: CaseClause -> String
unparseCaseClause (CaseClause q e) =
  "[(" ++ unwords (map unparseQuotation q) ++ ") " ++ unparseExpr e ++ "]"

unparseQuotation :: Quotation -> String
unparseQuotation (QuoteNumber n) = show n
unparseQuotation (QuoteBoolean True) = "#t"
unparseQuotation (QuoteBoolean False) = "#f"
unparseQuotation (QuoteChar ch) = "#\\" ++ [ch]
unparseQuotation (QuoteString s) = s
unparseQuotation (QuoteSymbol s) = s
unparseQuotation QuoteEmpty = "()"
unparseQuotation (QuotePair q1 q2) =
  "(" ++ unparseQuotation q1 ++ " . " ++ unparseQuotation q2 ++ ")"

unparseLetbinding :: LetBinding -> String
unparseLetbinding (LetBinding v e) =
  "[" ++ unparseVar v ++ " " ++ unparseExpr e ++ "]"

unparseLetrecBinding :: LetrecBinding -> String
unparseLetrecBinding (LetrecBinding v lambda) =
  "[" ++ unparseVar v ++ " " ++ unparseLambdaAbstraction lambda ++ "]"

unparseLambdaAbstraction :: LambdaAbstraction -> String
unparseLambdaAbstraction (Lambda vs e) =
  "(lambda (" ++ unwords (map unparseVar vs) ++
  ") " ++ unparseExpr e ++ ")"
unparseLambdaAbstraction (TraceLambda v vs e) =
  "(trace-lambda " ++ unparseVar v ++
  " (" ++ unwords (map unparseVar vs) ++
  ") " ++ unparseExpr e ++ ")"
