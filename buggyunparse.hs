module BuggyUnparse where
import QcSyntax
import Control.Monad.State
import Test.QuickCheck
import qualified System.Random as Rand
import qualified Data.List as List
import qualified Unparse


{-
  1. n = Number of nodes
  2. Enumerate nodes by 1, 2, ..., n
  3. Probability of syntax error at numbered k: sum i = 1 to k of 1/n
  3.5 Generate syntax error if: random(0, n) <= k
  4. If syntax error was generated: Normal unparse.
     Otherwise: Recursive buggy unparse
  5. ???
  6. Profit!
-}

concatMapM :: Monad m => (a -> m [b]) -> [a] -> m [b]
concatMapM _ [] = return []
concatMapM f (x:xs) = liftM2 (++) (f x) (concatMapM f xs)

numberOfNodes :: Program -> Int
numberOfNodes p = sum (List.map numberOfNodesToplevel p)

numberOfNodesToplevel :: ToplevelForm -> Int
numberOfNodesToplevel (ToplevelDefinition v e) =
  1 + numberOfNodesVar v + numberOfNodesExpression e
numberOfNodesToplevel (ToplevelExpression e) =
  1 + numberOfNodesExpression e

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
  sum [1, numberOfNodesExpressions [e1, e2]]
numberOfNodesCondClause (GuardedImpl e1 e2) =
  sum [1, numberOfNodesExpressions [e1, e2]]

numberOfNodesQuotation :: Quotation -> Int
numberOfNodesQuotation (QuoteNumber _) = 1
numberOfNodesQuotation (QuoteBoolean _) = 1
numberOfNodesQuotation (QuoteChar _) = 1
numberOfNodesQuotation (QuoteString _) = 1
numberOfNodesQuotation (QuoteSymbol _) = 1
numberOfNodesQuotation QuoteEmpty = 1
numberOfNodesQuotation (QuotePair q1 q2) =
  sum [1, numberOfNodesQuotation q1, numberOfNodesQuotation q2]

numberOfNodesCase :: CaseClause -> Int
numberOfNodesCase (CaseClause qs e) =
  sum [1, genericNumberOfNodes numberOfNodesQuotation qs, numberOfNodesExpression e]

numberOfNodesBindings :: [LetBinding] -> Int
numberOfNodesBindings = genericNumberOfNodes numberOfNodesBinding

numberOfNodesBinding :: LetBinding -> Int
numberOfNodesBinding (LetBinding v e) =
  sum [1, numberOfNodesVar v, numberOfNodesExpression e]

numberOfNodesRecBinding :: LetrecBinding -> Int
numberOfNodesRecBinding (LetrecBinding v lambda) =
  sum [1, numberOfNodesVar v, numberOfNodesLambda lambda]

numberOfNodesLambda :: LambdaAbstraction -> Int
numberOfNodesLambda (Lambda vs e) =
  sum [1, numberOfNodesVars vs, numberOfNodesExpression e]
numberOfNodesLambda (TraceLambda v vs e) =
  sum [1, numberOfNodesVars (v:vs), numberOfNodesExpression e]

numberOfNodesExpression :: Expression -> Int
numberOfNodesExpression (Number _) = 1
numberOfNodesExpression (Boolean _) = 1
numberOfNodesExpression (Ch _) = 1
numberOfNodesExpression (Str _) = 1
numberOfNodesExpression (Time e) = sum [1, numberOfNodesExpression e]
numberOfNodesExpression (If e1 e2 e3) =
  sum [1, numberOfNodesExpressions [e1, e2, e3]]
numberOfNodesExpression (And es) = sum [1, numberOfNodesExpressions es]
numberOfNodesExpression (Or es) = sum [1, numberOfNodesExpressions es]
numberOfNodesExpression (Cond clauses e) =
  sum [1, genericNumberOfNodes numberOfNodesCondClause clauses,
       numberOfNodesExpression e]
numberOfNodesExpression (Case e1 clauses e2) =
  sum [1, genericNumberOfNodes numberOfNodesCase clauses,
       numberOfNodesExpressions [e1, e2]]
numberOfNodesExpression (Let bindings e) =
  sum [1, numberOfNodesBindings bindings, numberOfNodesExpression e]
numberOfNodesExpression (Letstar bindings e) =
  sum [1, numberOfNodesBindings bindings, numberOfNodesExpression e]
numberOfNodesExpression (Letrec bindings e) =
  sum [1, genericNumberOfNodes numberOfNodesRecBinding bindings,
       numberOfNodesExpression e]
numberOfNodesExpression (Begin e1 e2) =
  sum [1, numberOfNodesExpressions [e1, e2]]
numberOfNodesExpression (Unless e1 e2) =
  sum [1, numberOfNodesExpressions [e1, e2]]
numberOfNodesExpression (Quote q) =
  sum [1, numberOfNodesQuotation q]
numberOfNodesExpression (LambdaExpression lambda) =
  sum [1, numberOfNodesLambda lambda]
numberOfNodesExpression (Application e1 e2) =
  sum [1, numberOfNodesExpressions [e1, e2]]

definition :: StateT St IO String
definition = do
  x <- lift $ generate genDefinition
  return $ Unparse.unparseToplevel x

number :: StateT St IO String
number = do
  x <- lift $ generate genNumber
  return $ Unparse.unparseExpr x

boolean :: StateT St IO String
boolean = do
  x <- lift $ generate genBoolean
  return $ Unparse.unparseExpr x
char :: StateT St IO String
char = do
  x <- lift $ generate genChar
  return $ Unparse.unparseExpr x
str :: StateT St IO String
str = do
  x <- lift $ generate genStr
  return $ Unparse.unparseExpr x
time :: StateT St IO String
time = do
  x <- lift $ generate (sized genTime)
  return $ Unparse.unparseExpr x
if_ :: StateT St IO String
if_ = do
  x <- lift $ generate (sized genIf)
  return $ Unparse.unparseExpr x
and_ :: StateT St IO String
and_ = do
  x <- lift $ generate (sized genAnd)
  return $ Unparse.unparseExpr x
or_ :: StateT St IO String
or_ = do
  x <- lift $ generate (sized genOr)
  return $ Unparse.unparseExpr x
cond :: StateT St IO String
cond = do
  x <- lift $ generate (sized genCond)
  return $ Unparse.unparseExpr x
case_ :: StateT St IO String
case_ = do
  x <- lift $ generate (sized genCase)
  return $ Unparse.unparseExpr x
let_ :: StateT St IO String
let_ = do
  x <- lift $ generate (sized genLet)
  return $ Unparse.unparseExpr x
letstar :: StateT St IO String
letstar = do
  x <- lift $ generate (sized genLetstar)
  return $ Unparse.unparseExpr x
letrec :: StateT St IO String
letrec = do
  x <- lift $ generate (sized genLetrec)
  return $ Unparse.unparseExpr x
begin :: StateT St IO String
begin = do
  x <- lift $ generate (sized genBegin)
  return $ Unparse.unparseExpr x
unless_ :: StateT St IO String
unless_ = do
  x <- lift $ generate (sized genUnless)
  return $ Unparse.unparseExpr x
quote :: StateT St IO String
quote = do
  x <- lift $ generate (sized genQuote)
  return $ Unparse.unparseExpr x
abstraction :: StateT St IO String
abstraction = do
  x <- lift $ generate (sized genLambdaAbstraction)
  return $ Unparse.unparseLambdaAbstraction x
application :: StateT St IO String
application = do
  x <- lift $ generate (sized genApplication)
  return $ Unparse.unparseExpr x
caseClause :: StateT St IO String
caseClause = do
  x <- lift $ generate (sized genCaseClause)
  return $ Unparse.unparseCaseClause x
condClause :: StateT St IO String
condClause = do
  x <- lift $ generate (sized genCondClause)
  return $ Unparse.unparseCondClause x
letBinding :: StateT St IO String
letBinding = do
  x <- lift $ generate (sized genLetBinding)
  return $ Unparse.unparseLetbinding x
letrecBinding :: StateT St IO String
letrecBinding = do
  x <- lift $ generate (sized genLetrecBinding)
  return $ Unparse.unparseLetrecBinding x

handle :: StateT St IO String -> StateT St IO String -> StateT St IO String
handle bad good = do
  err <- syntaxError
  if err then do
    lift $ putStrLn "BAD!"
    bad
  else good

data St = St { node :: Int, bound :: Int, randoms :: [Int], didErr :: Bool }

rand :: StateT St IO Int
rand = do
  rs <- gets randoms
  let ([r], rs') = List.splitAt 1 rs
  modify $ \st -> st {randoms = rs'}
  return r

syntaxError :: StateT St IO Bool
syntaxError = do
  d <- gets didErr
  if d then return False
  else do
    r <- rand
    k <- gets node
    lift $ print k
    modify $ \st -> st {didErr = r <= k}
    gets didErr

incr :: StateT St IO ()
incr = modify $ \st -> st {node = 1 + node st}

oneOf :: [StateT St IO String] -> StateT St IO String
oneOf xs = do
  idx <- lift $ Rand.randomRIO (0, List.length xs - 1)
  xs !! idx

before :: StateT St IO () -> StateT St IO b -> StateT St IO b
before pre post = do
  pre
  post

unparse :: Program -> IO String
unparse p = do
  stdgen <- Rand.getStdGen
  let rs = Rand.randomRs (0, n) stdgen
      initial = St {node = 0, bound = n, randoms = rs, didErr = False}
  putStrLn ("n is " ++ show n)
  evalStateT (concatMapM unparseToplevel p) initial
  where n = numberOfNodes p

unparseToplevel :: ToplevelForm -> StateT St IO String
unparseToplevel (ToplevelDefinition var e) = do
  incr
  svar <- handle noUnparseVar (unparseVar var)
  se <- handle noUnparseExpression (unparseExpr e)
  return $ "(define " ++ svar ++ " " ++ se ++ ")\n"
unparseToplevel (ToplevelExpression e) = incr `before` unparseExpr e

unparseVar :: Variable -> StateT St IO String
unparseVar v = do
  incr
  handle (return "1") (return $ Unparse.unparseVar v)

noUnparseVar :: StateT St IO String
noUnparseVar =
  oneOf [definition, number, boolean, str, time, if_, and_,
         or_, cond, case_, let_, letstar, letrec, begin,
         unless_, quote, abstraction, application, caseClause,
         condClause, letBinding, letrecBinding, return ""]

noUnparseExpression :: StateT St IO String
noUnparseExpression =
  oneOf [condClause, caseClause, letBinding, letrecBinding, return ""]

noUnparseCondClause :: StateT St IO String
noUnparseCondClause =
  oneOf [definition, number, boolean, str, time, if_, and_,
         or_, cond, case_, let_, letstar, letrec, begin,
         unless_, quote, abstraction, application, caseClause,
         letBinding, letrecBinding, return ""]

noUnparseCaseClause :: StateT St IO String
noUnparseCaseClause =
 oneOf [definition, number, boolean, str, time, if_, and_,
        or_, cond, case_, let_, letstar, letrec, begin,
        unless_, quote, abstraction, application, condClause,
        letBinding, letrecBinding, return ""]

noUnparseQuotation :: StateT St IO String
noUnparseQuotation =
  oneOf [definition, time, if_, and_, or_, cond, case_,
         let_, letstar, letrec, begin, unless_, quote, abstraction,
         application, caseClause, condClause, letBinding,
         letrecBinding, return ""]

noUnparseLetbinding :: StateT St IO String
noUnparseLetbinding =
  oneOf [definition, time, if_, and_, or_, cond, case_,
         let_, letstar, letrec, begin, unless_, quote, abstraction,
         application, caseClause, condClause, return ""]

noUnparseLetrecBinding :: StateT St IO String
noUnparseLetrecBinding =
  oneOf [definition, time, if_, and_, or_, cond, case_,
         let_, letstar, letrec, begin, unless_, quote, abstraction,
         application, caseClause, condClause, return ""]

noUnparseAbstraction :: StateT St IO String
noUnparseAbstraction =
  oneOf [definition, time, if_, and_, or_, cond, case_,
         let_, letstar, letrec, begin, unless_, quote, letBinding,
         letrecBinding, application, caseClause, condClause, return ""]

unparseExpr :: Expression -> StateT St IO String
unparseExpr (Number n) =
  incr `before` handle noUnparseExpression
                 (return $ Unparse.unparseExpr (Number n))
unparseExpr (Boolean b) =
  incr `before` handle noUnparseExpression
                 (return $ Unparse.unparseExpr (Boolean b))
unparseExpr (Ch ch) =
  incr `before` handle noUnparseExpression
                 (return $ Unparse.unparseExpr (Ch ch))
unparseExpr (Str s) =
  incr `before` handle noUnparseExpression
                 (return $ Unparse.unparseExpr (Str s))
unparseExpr (Time e) = do
  incr
  se <- handle noUnparseExpression (unparseExpr e)
  return $ "(time " ++ se ++ ")"
unparseExpr (If e1 e2 e3) = do
  incr
  se1 <- handle noUnparseExpression (unparseExpr e1)
  se2 <- handle noUnparseExpression (unparseExpr e2)
  se3 <- handle noUnparseExpression (unparseExpr e3)
  return $ "(if " ++ se1 ++ " " ++ se2 ++ " " ++ se3 ++ ")"
unparseExpr (And es) = do
  incr
  ses <- mapM unparse es
  return $ "(and " ++ unwords ses ++ ")"
  where unparse = handle noUnparseExpression . unparseExpr
unparseExpr (Or es) = do
  incr
  ses <- mapM unparse es
  return $ "(or " ++ unwords ses ++ ")"
  where unparse = handle noUnparseExpression . unparseExpr
unparseExpr (Cond clauses e) = do
  incr
  sclauses <- mapM unparse clauses
  se <- unparseExpr e
  return $ "(cond " ++ unwords sclauses ++ " [else " ++ se ++ "])"
  where unparse = handle noUnparseCondClause . unparseCondClause
unparseExpr (Case e1 clauses e2) = do
  incr
  se1 <- handle noUnparseExpression (unparseExpr e1)
  se2 <- handle noUnparseExpression (unparseExpr e2)
  sclauses <- mapM unparse clauses
  return $ "(case " ++ se1 ++
           " " ++ unwords sclauses ++
           " [else " ++ se2 ++ "])"
  where unparse = handle noUnparseCaseClause . unparseCaseClause
unparseExpr (Let bindings e) = do
  incr
  sbindings <- mapM unparse bindings
  se <- handle noUnparseExpression (unparseExpr e)
  return $ "(let (" ++ unwords sbindings ++ ") " ++ se ++ ")"
  where unparse = handle noUnparseLetbinding . unparseLetbinding
unparseExpr (Letstar bindings e) = do
  incr
  sbindings <- mapM unparse bindings
  se <- handle noUnparseExpression (unparseExpr e)
  return $ "(let* (" ++ unwords sbindings ++ ") " ++ se ++ ")"
  where unparse = handle noUnparseLetbinding . unparseLetbinding
unparseExpr (Letrec bindings e) = do
  incr
  sbindings <- mapM unparse bindings
  se <- handle noUnparseExpression (unparseExpr e)
  return $ "(let* (" ++ unwords sbindings ++ ") " ++ se ++ ")"
  where unparse = handle noUnparseLetrecBinding . unparseLetrecBinding
unparseExpr (Begin e1 e2) = do
  incr
  se1 <- handle noUnparseExpression (unparseExpr e1)
  se2 <- handle noUnparseExpression (unparseExpr e2)
  return $ "(begin " ++ se1 ++ " " ++ se2 ++ ")"
unparseExpr (Unless e1 e2) = do
  incr
  se1 <- handle noUnparseExpression (unparseExpr e1)
  se2 <- handle noUnparseExpression (unparseExpr e2)
  return $ "(unless " ++ se1 ++ " " ++ se2 ++ ")"
unparseExpr (Quote q) = do
  incr
  sq <- handle noUnparseQuotation (unparseQuotation q)
  return $ "(quote " ++ sq ++ ")"
unparseExpr (LambdaExpression lambda) =
  incr `before` handle noUnparseAbstraction (unparseLambdaAbstraction lambda)
unparseExpr (Application e1 e2) = do
  incr
  se1 <- handle noUnparseExpression (unparseExpr e1)
  se2 <- handle noUnparseExpression (unparseExpr e2)
  return $ "(" ++ se1 ++ " " ++ se2 ++ ")"

unparseCondClause :: CondClause -> StateT St IO String
unparseCondClause (Immediate e) = do
  incr
  se <- handle noUnparseExpression (unparseExpr e)
  return $ "[" ++ se ++ "]"
unparseCondClause (Guarded e1 e2) = do
  incr
  se1 <- handle noUnparseExpression (unparseExpr e1)
  se2 <- handle noUnparseExpression (unparseExpr e2)
  return $ "[" ++ se1 ++ " " ++ se2 ++ "]"
unparseCondClause (GuardedImpl e1 e2) = do
  incr
  se1 <- handle noUnparseExpression (unparseExpr e1)
  se2 <- handle noUnparseExpression (unparseExpr e2)
  return $ "[" ++ se1 ++ " => " ++ se2 ++ "]"

unparseCaseClause :: CaseClause -> StateT St IO String
unparseCaseClause (CaseClause qs e) = do
  incr
  sqs <- mapM unparse qs
  se <- handle noUnparseExpression (unparseExpr e)
  return $ "[(" ++ unwords sqs ++ ") " ++ se ++ "]"
  where unparse = handle noUnparseQuotation . unparseQuotation

unparseQuotation :: Quotation -> StateT St IO String
unparseQuotation (QuoteNumber n) =
  incr `before` handle noUnparseQuotation
                 (return $ Unparse.unparseQuotation (QuoteNumber n))
unparseQuotation (QuoteBoolean b) =
  incr `before` handle noUnparseQuotation
                 (return $ Unparse.unparseQuotation (QuoteBoolean b))
unparseQuotation (QuoteChar ch) =
  incr `before` handle noUnparseQuotation
                 (return $ Unparse.unparseQuotation (QuoteChar ch))
unparseQuotation (QuoteString s) =
  incr `before` handle noUnparseQuotation
                 (return $ Unparse.unparseQuotation (QuoteString s))
unparseQuotation (QuoteSymbol s) =
  incr `before` handle noUnparseQuotation
                 (return $ Unparse.unparseQuotation (QuoteSymbol s))
unparseQuotation QuoteEmpty =
  incr `before` handle noUnparseQuotation
                 (return $ Unparse.unparseQuotation QuoteEmpty)
unparseQuotation (QuotePair q1 q2) = do
  incr
  sq1 <- unparseQuotation q1
  sq2 <- unparseQuotation q2
  return $ "(" ++ sq1 ++ " . " ++ sq2 ++ ")"

unparseLetbinding :: LetBinding -> StateT St IO String
unparseLetbinding (LetBinding v e) = do
  incr
  sv <- handle noUnparseVar (unparseVar v)
  se <- handle noUnparseExpression (unparseExpr e)
  return $ "[" ++ sv ++ " " ++ se ++ "]"

unparseLetrecBinding :: LetrecBinding -> StateT St IO String
unparseLetrecBinding (LetrecBinding v lambda) = do
  incr
  sv <- handle noUnparseVar (unparseVar v)
  slambda <- handle noUnparseAbstraction (unparseLambdaAbstraction lambda)
  return $ "[" ++ sv ++ " " ++ slambda ++ "]"

unparseLambdaAbstraction :: LambdaAbstraction -> StateT St IO String
unparseLambdaAbstraction (Lambda vs e) = do
  incr
  svs <- mapM unparse vs
  se <- handle noUnparseExpression (unparseExpr e)
  return $ "(lambda (" ++ unwords svs ++ ") " ++ se ++ ")"
  where unparse = handle noUnparseVar . unparseVar
unparseLambdaAbstraction (TraceLambda v vs e) = do
  incr
  sv <- handle noUnparseVar (unparseVar v)
  svs <- mapM unparse vs
  se <- handle noUnparseExpression (unparseExpr e)
  return $ "(trace-lambda " ++ sv ++ " (" ++ unwords svs ++ ") " ++ se ++ ")"
  where unparse = handle noUnparseVar . unparseVar
