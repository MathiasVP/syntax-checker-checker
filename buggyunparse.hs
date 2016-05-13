module BuggyUnparse where
import QcSyntax
import Control.Monad.State
import Test.QuickCheck hiding (generate)
import Test.QuickCheck.Gen hiding (generate)
import Test.QuickCheck.Random
import qualified System.Random as Rand
import qualified Data.List as List
import qualified Unparse

data St = St { current :: Int, target :: Int, rand :: Rand.StdGen }

generate :: Gen a -> State St a
generate g = do
  r <- gets rand
  let (n, r') = Rand.random r
  modify $ \ st -> st { rand = r' }
  return $ unGen g (mkQCGen n) n

definition :: State St String
definition = do
  x <- generate genDefinition
  return $ Unparse.unparseToplevel x

number :: State St String
number = do
  x <- generate genNumber
  return $ Unparse.unparseExpr x

boolean :: State St String
boolean = do
  x <- generate genBoolean
  return $ Unparse.unparseExpr x
char :: State St String
char = do
  x <- generate genChar
  return $ Unparse.unparseExpr x
str :: State St String
str = do
  x <- generate genStr
  return $ Unparse.unparseExpr x
time :: State St String
time = do
  x <- generate (sized genTime)
  return $ Unparse.unparseExpr x
if_ :: State St String
if_ = do
  x <- generate (sized genIf)
  return $ Unparse.unparseExpr x
and_ :: State St String
and_ = do
  x <- generate (sized genAnd)
  return $ Unparse.unparseExpr x
or_ :: State St String
or_ = do
  x <- generate (sized genOr)
  return $ Unparse.unparseExpr x
cond :: State St String
cond = do
  x <- generate (sized genCond)
  return $ Unparse.unparseExpr x
case_ :: State St String
case_ = do
  x <- generate (sized genCase)
  return $ Unparse.unparseExpr x
let_ :: State St String
let_ = do
  x <- generate (sized genLet)
  return $ Unparse.unparseExpr x
letstar :: State St String
letstar = do
  x <- generate (sized genLetstar)
  return $ Unparse.unparseExpr x
letrec :: State St String
letrec = do
  x <- generate (sized genLetrec)
  return $ Unparse.unparseExpr x
begin :: State St String
begin = do
  x <- generate (sized genBegin)
  return $ Unparse.unparseExpr x
unless_ :: State St String
unless_ = do
  x <- generate (sized genUnless)
  return $ Unparse.unparseExpr x
quote :: State St String
quote = do
  x <- generate (sized genQuote)
  return $ Unparse.unparseExpr x
abstraction :: State St String
abstraction = do
  x <- generate (sized genLambdaAbstraction)
  return $ Unparse.unparseLambdaAbstraction x
application :: State St String
application = do
  x <- generate (sized genApplication)
  return $ Unparse.unparseExpr x
caseClause :: State St String
caseClause = do
  x <- generate (sized genCaseClause)
  return $ Unparse.unparseCaseClause x
condClause :: State St String
condClause = do
  x <- generate (sized genCondClause)
  return $ Unparse.unparseCondClause x
letBinding :: State St String
letBinding = do
  x <- generate (sized genLetBinding)
  return $ Unparse.unparseLetbinding x
letrecBinding :: State St String
letrecBinding = do
  x <- generate (sized genLetrecBinding)
  return $ Unparse.unparseLetrecBinding x

incr :: State St ()
incr = modify $ \st -> st {current = 1 + current st}

oneOf :: [State St String] -> State St String
oneOf xs = do
  r <- gets rand
  let (idx, r') = Rand.randomR (0, List.length xs - 1) r
  modify $ \st -> st { rand = r' }
  xs !! idx

concatMapM :: Monad m => (a -> m [b]) -> [a] -> m [b]
concatMapM _ [] = return []
concatMapM f (x:xs) = liftM2 (++) (f x) (concatMapM f xs)

shouldErr :: State St Bool
shouldErr = do
  c <- gets current
  t <- gets target
  return $ c == t

unparse :: Program -> Int -> Int -> String
unparse p point seed =
  evalState (concatMapM unparseToplevel p) initialSt
  where initialSt = St { current = 0
                       , target = point
                       , rand = Rand.mkStdGen seed }

handle :: [State St String] -> State St String -> State St String
handle invalids valid = do
  b <- shouldErr
  if b then oneOf invalids
  else valid

unparseToplevel :: ToplevelForm -> State St String
unparseToplevel (ToplevelDefinition var e) = do
  incr
  svar <- handle [return "", definition, number, boolean, char, str,
                  time, if_, and_, or_, cond, case_, let_, letstar,
                  letrec, begin, unless_, quote, abstraction, application,
                  caseClause, condClause, letBinding, letrecBinding]
                  (unparseVar var)
  incr
  se <- handle [return "", definition] (unparseExpr e)
  return $ "(define " ++ svar ++ " " ++ se ++ ")\n"
unparseToplevel (ToplevelExpression e) = unparseExpr e

unparseVar :: Variable -> State St String
unparseVar v = do
  incr
  handle [definition, number, boolean, char, str,
          time, if_, and_, or_, cond, case_, let_, letstar,
          letrec, begin, unless_, quote, abstraction, application,
          caseClause, condClause, letBinding, letrecBinding]
          (return $ Unparse.unparseVar v)

unparseExpr :: Expression -> State St String
unparseExpr (Number n) = do
  incr
  handle [definition] (return $ Unparse.unparseExpr (Number n))
unparseExpr (Boolean b) = do
  incr
  handle [definition] (return $ Unparse.unparseExpr (Boolean b))
unparseExpr (Ch ch) = do
  incr
  handle [definition] (return $ Unparse.unparseExpr (Ch ch))
unparseExpr (Str s) = do
  incr
  handle [definition] (return $ Unparse.unparseExpr (Str s))
unparseExpr (Time e) = do
  incr
  se <- handle [definition] (unparseExpr e)
  return $ "(time " ++ se ++ ")"
unparseExpr (If e1 e2 e3) = do
  incr
  se1 <- handle [return "", definition] (unparseExpr e1)
  incr
  se2 <- handle [return "", definition] (unparseExpr e2)
  incr
  se3 <- handle [return "", definition] (unparseExpr e3)
  return $ "(if " ++ se1 ++ " " ++ se2 ++ " " ++ se3 ++ ")"
unparseExpr (And es) = do
  ses <- mapM unparse es
  return $ "(and " ++ unwords ses ++ ")"
  where unparse = handle [definition] . unparseExpr
unparseExpr (Or es) = do
  ses <- mapM unparse es
  return $ "(or " ++ unwords ses ++ ")"
  where unparse = handle [definition] . unparseExpr
unparseExpr (Cond clauses e) = do
  sclauses <- mapM unparse clauses
  incr
  se <- handle [definition] (unparseExpr e)
  incr
  handle [return $ "(cond " ++ unwords sclauses ++ ")"] (return $ "(cond " ++ unwords sclauses ++ " [else " ++ se ++ "])")
  where unparse = handle [definition] . unparseCondClause
unparseExpr (Case e1 clauses e2) = do
  incr
  se1 <- handle [return "", definition] (unparseExpr e1)
  incr
  se2 <- handle [return "", definition] (unparseExpr e2)
  sclauses <- mapM unparse clauses
  incr
  let valid = "(case " ++ se1 ++
              " " ++ unwords sclauses ++
              " [else " ++ se2 ++ "])"
  handle [return $ "(case " ++ se1 ++ " " ++ unwords sclauses ++ ")"]
         (return valid)
  where unparse =
          handle [definition, time, if_, and_, or_, cond, case_, let_,
                  letstar, letrec, begin, unless_, quote, abstraction,
                  application, caseClause, condClause, letBinding,
                  letrecBinding] . unparseCaseClause
unparseExpr (Let bindings e) = do
  sbindings <- mapM unparse bindings
  incr
  se <- handle [return "", definition] (unparseExpr e)
  return $ "(let (" ++ unwords sbindings ++ ") " ++ se ++ ")"
  where unparse = handle [definition, time, if_, and_, or_,
                          cond, case_, let_, letstar, letrec, begin,
                          unless_, quote, abstraction] . unparseLetbinding
unparseExpr (Letstar bindings e) = do
  sbindings <- mapM unparse bindings
  incr
  se <- handle [return "", definition] (unparseExpr e)
  incr
  return $ "(let* (" ++ unwords sbindings ++ ") " ++ se ++ ")"
  where unparse = handle [definition, time, if_, and_, or_,
                          cond, case_, let_, letstar, letrec, begin,
                          unless_, quote, abstraction] . unparseLetbinding
unparseExpr (Letrec bindings e) = do
  sbindings <- mapM unparse bindings
  incr
  se <- handle [return "", definition] (unparseExpr e)
  return $ "(letrec (" ++ unwords sbindings ++ ") " ++ se ++ ")"
  where unparse = handle [definition, time, if_, and_, or_,
                          cond, case_, let_, letstar, letrec, begin,
                          unless_, quote, abstraction] . unparseLetrecBinding
unparseExpr (Begin e1 e2) = do
  incr
  se1 <- handle [return "", definition] (unparseExpr e1)
  incr
  se2 <- handle [return "", definition] (unparseExpr e2)
  return $ "(begin " ++ se1 ++ " " ++ se2 ++ ")"
unparseExpr (Unless e1 e2) = do
  incr
  se1 <- handle [return "", definition] (unparseExpr e1)
  incr
  se2 <- handle [return "", definition] (unparseExpr e2)
  return $ "(unless " ++ se1 ++ " " ++ se2 ++ ")"
unparseExpr (Quote q) = do
  incr
  sq <- handle [return "", time, if_, and_, or_, cond, case_, let_,
                letstar, letrec, begin, unless_, abstraction,
                application] (unparseQuotation q)
  return $ "(quote " ++ sq ++ ")"
unparseExpr (LambdaExpression lambda) =
  unparseLambdaAbstraction lambda
unparseExpr (Application e1 e2) = do
  incr
  se1 <- handle [return "if", return "define", return "begin", return "unless"]
                (unparseExpr e1)
  incr
  se2 <- handle [return "if", return "define", return "begin", return "unless"]
                (unparseExpr e2)
  return $ "(" ++ se1 ++ " " ++ se2 ++ ")"

unparseCondClause :: CondClause -> State St String
unparseCondClause (Immediate e) = do
  incr
  se <- handle [return "", definition] (unparseExpr e)
  return $ "[" ++ se ++ "]"
unparseCondClause (Guarded e1 e2) = do
  incr
  se1 <- handle [definition] (unparseExpr e1)
  incr
  se2 <- handle [definition] (unparseExpr e2)
  return $ "[" ++ se1 ++ " " ++ se2 ++ "]"
unparseCondClause (GuardedImpl e1 e2) = do
  incr
  se1 <- handle [return "", definition] (unparseExpr e1)
  incr
  se2 <- handle [return "", definition] (unparseExpr e2)
  return $ "[" ++ se1 ++ " => " ++ se2 ++ "]"

unparseCaseClause :: CaseClause -> State St String
unparseCaseClause (CaseClause qs e) = do
  sqs <- mapM unparse qs
  incr
  se <- handle [return "", definition] (unparseExpr e)
  return $ "[(" ++ unwords sqs ++ ") " ++ se ++ "]"
  where unparse = handle [return "", time, if_, and_, or_,
                          cond, case_, let_, letstar, letrec,
                          begin, unless_, abstraction, application]
                          . unparseQuotation

unparseQuotation :: Quotation -> State St String
unparseQuotation (QuoteNumber n) = do
  incr
  handle [return "", time, if_, and_, or_,
          cond, case_, let_, letstar, letrec,
          begin, unless_, abstraction, application]
         (return $ Unparse.unparseQuotation (QuoteNumber n))
unparseQuotation (QuoteBoolean b) = do
  incr
  handle [return "", time, if_, and_, or_,
          cond, case_, let_, letstar, letrec,
          begin, unless_, abstraction, application]
         (return $ Unparse.unparseQuotation (QuoteBoolean b))
unparseQuotation (QuoteChar ch) = do
  incr
  handle [return "", time, if_, and_, or_,
          cond, case_, let_, letstar, letrec,
          begin, unless_, abstraction, application]
         (return $ Unparse.unparseQuotation (QuoteChar ch))
unparseQuotation (QuoteString s) = do
  incr
  handle [return "", time, if_, and_, or_,
          cond, case_, let_, letstar, letrec,
          begin, unless_, abstraction, application]
         (return $ Unparse.unparseQuotation (QuoteString s))
unparseQuotation (QuoteSymbol s) = do
  incr
  handle [return "", time, if_, and_, or_,
          cond, case_, let_, letstar, letrec,
          begin, unless_, abstraction, application]
         (return $ Unparse.unparseQuotation (QuoteSymbol s))
unparseQuotation QuoteEmpty = do
  incr
  handle [return "", time, if_, and_, or_,
          cond, case_, let_, letstar, letrec,
          begin, unless_, abstraction, application]
         (return $ Unparse.unparseQuotation QuoteEmpty)
unparseQuotation (QuotePair q1 q2) = do
  incr
  sq1 <- handle [return "", time, if_, and_, or_,
                 cond, case_, let_, letstar, letrec,
                 begin, unless_, abstraction, application]
                 (unparseQuotation q1)
  incr
  sq2 <- handle [return "", time, if_, and_, or_,
                 cond, case_, let_, letstar, letrec,
                 begin, unless_, abstraction, application]
                 (unparseQuotation q2)
  return $ "(" ++ sq1 ++ " . " ++ sq2 ++ ")"

unparseLetbinding :: LetBinding -> State St String
unparseLetbinding (LetBinding v e) = do
  incr
  sv <- handle [definition, number, boolean, char, str,
          time, if_, and_, or_, cond, case_, let_, letstar,
          letrec, begin, unless_, quote, abstraction, application,
          caseClause, condClause, letBinding, letrecBinding] (unparseVar v)
  incr
  se <- handle [return "", definition] (unparseExpr e)
  return $ "[" ++ sv ++ " " ++ se ++ "]"

unparseLetrecBinding :: LetrecBinding -> State St String
unparseLetrecBinding (LetrecBinding v lambda) = do
  incr
  sv <- handle [return "", definition, number, boolean, char, str,
          time, if_, and_, or_, cond, case_, let_, letstar,
          letrec, begin, unless_, quote, abstraction, application,
          caseClause, condClause, letBinding, letrecBinding] (unparseVar v)
  incr
  slambda <- handle [return "", definition, number, boolean, char, str,
                     time, if_, and_, or_, cond, case_, let_, letstar,
                     letrec, begin, unless_, quote, application, caseClause,
                     condClause, letBinding, letrecBinding]
                     (unparseLambdaAbstraction lambda)
  return $ "[" ++ sv ++ " " ++ slambda ++ "]"

unparseLambdaAbstraction :: LambdaAbstraction -> State St String
unparseLambdaAbstraction (Lambda vs e) = do
  svs <- mapM unparse vs
  incr
  se <- handle [return "", definition] (unparseExpr e)
  return $ "(lambda (" ++ unwords svs ++ ") " ++ se ++ ")"
  where unparse = handle [return "", definition, number, boolean, char, str,
                          time, if_, and_, or_, cond, case_, let_, letstar,
                          letrec, begin, unless_, quote, abstraction,
                          application, caseClause, condClause, letBinding,
                          letrecBinding] . unparseVar
unparseLambdaAbstraction (TraceLambda v vs e) = do
  incr
  sv <- handle [return "", definition, number, boolean, char, str,
                time, if_, and_, or_, cond, case_, let_, letstar,
                letrec, begin, unless_, quote, abstraction,
                application, caseClause, condClause, letBinding,
                letrecBinding] (unparseVar v)
  svs <- mapM unparse vs
  incr
  se <- handle [return "", definition] (unparseExpr e)
  return $ "(trace-lambda " ++ sv ++ " (" ++ unwords svs ++ ") " ++ se ++ ")"
  where unparse = handle [return "", definition, number, boolean, char, str,
                          time, if_, and_, or_, cond, case_, let_, letstar,
                          letrec, begin, unless_, quote, abstraction,
                          application, caseClause, condClause, letBinding,
                          letrecBinding] . unparseVar
