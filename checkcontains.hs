module CheckContains where

import QcSyntax
import qualified Data.List as List

data Accept
  = Accept {
      topLevelForm :: Bool,
      topLevelDefinition :: Bool,
      topLevelExpression :: Bool,

      expression :: Bool,
      number :: Bool,
      boolean :: Bool,
      ch :: Bool,
      str :: Bool,
      time :: Bool,
      if_ :: Bool,
      and_ :: Bool,
      or_ :: Bool,
      cond :: Bool,
      case_ :: Bool,
      let_ :: Bool,
      letstar :: Bool,
      letrec :: Bool,
      begin :: Bool,
      unless :: Bool,
      quote :: Bool,
      application :: Bool,

      variable :: Bool,

      condClauseImmediate :: Bool,
      condClauseGuarded :: Bool,
      condClauseguardedImpl :: Bool,

      caseClause :: Bool,

      letBinding :: Bool,
      letstarBinding :: Bool,
      letrecBinding :: Bool,

      lambda :: Bool,
      traceLambda :: Bool,

      quoteNumber :: Bool,
      quoteBoolean :: Bool,
      quoteChar :: Bool,
      quoteString :: Bool,
      quoteSymbol :: Bool,
      quoteEmpty :: Bool,
      quotePair :: Bool
    }

acceptAll :: Accept
acceptAll = Accept {
  topLevelForm = True,
  topLevelDefinition = True,
  topLevelExpression = True,

  expression = True,
  number = True,
  boolean = True,
  ch = True,
  str = True,
  time = True,
  if_ = True,
  and_ = True,
  or_ = True,
  cond = True,
  case_ = True,
  let_ = True,
  letstar = True,
  letrec = True,
  begin = True,
  unless = True,
  quote = True,
  application = True,

  variable = True,

  condClauseImmediate = True,
  condClauseGuarded = True,
  condClauseguardedImpl = True,

  caseClause = True,

  letBinding = True,
  letstarBinding = True,
  letrecBinding = True,

  lambda = True,
  traceLambda = True,

  quoteNumber = True,
  quoteBoolean = True,
  quoteChar = True,
  quoteString = True,
  quoteSymbol = True,
  quoteEmpty = True,
  quotePair = True
}

accepted :: Accept -> Program -> Bool
accepted a = acc
  where
    check :: [Accept -> Bool] -> Bool
    check [] = True
    check (p:ps) = p a && check ps

    acc :: Program -> Bool
    acc = List.all accToplevel

    accToplevel :: ToplevelForm -> Bool
    accToplevel (ToplevelDefinition _ e) =
      check [topLevelDefinition, variable, expression] && accExpression e
    accToplevel (ToplevelExpression e) = check [expression] && accExpression e

    accExpression :: Expression -> Bool
    accExpression (Number _) = check [number]
    accExpression (Boolean _) = check [boolean]
    accExpression (Ch _) = check [ch]
    accExpression (Str _) = check [str]
    accExpression (Time e) = check [time, expression] && accExpression e
    accExpression (If e1 e2 e3) =
      check [if_, expression] && List.all accExpression [e1, e2, e3]
    accExpression (And es) =
      check [and_] && List.all accExpression es
    accExpression (Or es) =
      check [or_] && List.all accExpression es
    accExpression (Cond clauses e) =
      check [cond, expression] &&
      List.all accCondClause clauses && accExpression e
    accExpression (Case e1 clauses e2) =
      check [case_, expression] &&
      accExpression e1 &&
      accExpression e2 &&
      List.all accCaseClause clauses
    accExpression (Let bindings e) =
      check [let_, expression] &&
      accExpression e &&
      List.all accLetbinding bindings
    accExpression (Letstar bindings e) =
      check [letstar, expression] &&
      List.all accLetbinding bindings &&
      accExpression e
    accExpression (Letrec bindings e) =
      check [letrec, expression] &&
      List.all accLetrecBinding bindings &&
      accExpression e
    accExpression (Begin e1 e2) =
      check [begin, expression] &&
      List.all accExpression [e1, e2]
    accExpression (Unless e1 e2) =
      check [unless, expression] &&
      List.all accExpression [e1, e2]
    accExpression (Quote q) =
      check [quote] && accQuotation q
    accExpression (LambdaExpression (Lambda _ e)) =
      check [lambda, variable, expression] && accExpression e
    accExpression (LambdaExpression (TraceLambda _ _ e)) =
      check [traceLambda, variable, expression] && accExpression e
    accExpression (Application e1 e2) =
      check [application] &&
      List.all accExpression [e1, e2]

    accCondClause :: CondClause -> Bool
    accCondClause (Immediate e) =
      check [condClauseImmediate, expression] && accExpression e
    accCondClause (Guarded e1 e2) =
      check [condClauseGuarded, expression] &&
      List.all accExpression [e1, e2]
    accCondClause (GuardedImpl e1 e2) =
      check [condClauseguardedImpl, expression] &&
      List.all accExpression [e1, e2]

    accCaseClause :: CaseClause -> Bool
    accCaseClause (CaseClause qs e) =
      check [caseClause, quote, expression] &&
      accExpression e &&
      List.all accQuotation qs

    accLetbinding :: LetBinding -> Bool
    accLetbinding (LetBinding _ e) =
      check [letBinding, variable, expression] &&
      accExpression e

    accLetrecBinding :: LetrecBinding -> Bool
    accLetrecBinding (LetrecBinding _ (Lambda _ e)) =
      check [letrecBinding, variable, expression, lambda] &&
      accExpression e
    accLetrecBinding (LetrecBinding _ (TraceLambda _ _ e)) =
        check [letrecBinding, variable, expression, traceLambda] &&
        accExpression e


    accQuotation :: Quotation -> Bool
    accQuotation (QuoteNumber _) = check [quoteNumber]
    accQuotation (QuoteBoolean _) = check [quoteBoolean]
    accQuotation (QuoteChar _) = check [quoteChar]
    accQuotation (QuoteString _) = check [quoteString]
    accQuotation (QuoteSymbol _) = check [quoteSymbol]
    accQuotation QuoteEmpty = check [quoteEmpty]
    accQuotation (QuotePair q1 q2) =
      check [quotePair] && List.all accQuotation [q1, q2]
