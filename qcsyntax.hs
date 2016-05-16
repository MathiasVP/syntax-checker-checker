module QcSyntax where

import Control.Monad
import Control.Applicative()
import Test.QuickCheck
import qualified Data.List as List

type Program = ToplevelForms
type ToplevelForms = [ToplevelForm]
data ToplevelForm
  = ToplevelDefinition Variable Expression
  | ToplevelExpression Expression
  deriving (Show, Read)

data Expression
  = Number Integer
  | Boolean Bool
  | Ch Char
  | Str String
  | Time Expression
  | If Expression Expression Expression
  | And [Expression]
  | Or [Expression]
  | Cond [CondClause] Expression
  | Case Expression [CaseClause] Expression
  | Let [LetBinding] Expression
  | Letstar [LetBinding] Expression
  | Letrec [LetrecBinding] Expression
  | Begin Expression Expression
  | Unless Expression Expression
  | Quote Quotation
  | LambdaExpression LambdaAbstraction
  | Application Expression Expression
  deriving (Show, Read)

newtype Variable = Variable String
  deriving (Show, Eq, Read)

instance Arbitrary Variable where
  arbitrary = sized genVariable

data CondClause
  = Immediate Expression
  | Guarded Expression Expression
  | GuardedImpl Expression Expression
  deriving (Show, Read)

data CaseClause
 = CaseClause [Quotation] Expression
 deriving (Show, Read)

data LetBinding
  = LetBinding Variable Expression
  deriving (Show, Read)

data LetrecBinding
  = LetrecBinding Variable LambdaAbstraction
  deriving (Show, Read)

data LambdaAbstraction
  = Lambda [Variable] Expression
  | TraceLambda Variable [Variable] Expression
  deriving (Show, Read)

data Quotation
  = QuoteNumber Integer
  | QuoteBoolean Bool
  | QuoteChar Char
  | QuoteString String
  | QuoteSymbol String
  | QuoteEmpty
  | QuotePair Quotation Quotation
  deriving (Show, Read)

instance Arbitrary ToplevelForm where
  arbitrary = sized genToplevelForm
  shrink = shrinkToplevelForm

shrinkToplevelForm :: ToplevelForm -> [ToplevelForm]
shrinkToplevelForm (ToplevelDefinition v e) =
  [ToplevelDefinition v e' | e' <- shrink e]
shrinkToplevelForm (ToplevelExpression e) =
  [ToplevelExpression e' | e' <- shrink e]

genToplevelForm :: Int -> Gen ToplevelForm
genToplevelForm size = oneof [ genDefinition
                             , fmap ToplevelExpression (genExpression size)]

genDefinition :: Gen ToplevelForm
genDefinition = do
  var <- arbitrary
  e <- arbitrary
  return $ ToplevelDefinition var e

genVariable :: Int -> Gen Variable
genVariable size =
  Variable <$> suchThat (genString size) (not . (`List.elem` keywords))
  where keywords = ["define", "time", "if", "cond", "else",
                    "case", "and", "or", "let", "let*",
                    "letrec", "begin", "quote", "lambda",
                    "trace-lambda"]

instance Arbitrary Expression where
  arbitrary = sized genExpression
  shrink = shrinkExpression

distinct :: [Variable] -> Bool
distinct vs =
  List.nub vs == vs

genExpression :: Int -> Gen Expression
genExpression 0 = oneof [genNumber, genBoolean, genChar, genStr]
genExpression size = oneof [ genNumber
                           , genBoolean
                           , genChar
                           , genStr
                           , genTime size
                           , genIf size
                           , genAnd size
                           , genOr size
                           , genCond size
                           , genCase size
                           , genLet size
                           , genLetstar size
                           , genLetrec size
                           , genBegin size
                           , genUnless size
                           , genQuote size
                           , fmap LambdaExpression (genLambdaAbstraction size)
                           , genApplication size ]

genNumber :: Gen Expression
genNumber = fmap Number arbitrary

genBoolean :: Gen Expression
genBoolean = fmap Boolean arbitrary

genChar :: Gen Expression
genChar = fmap Ch (elements ['a'..'z'])

genStr :: Gen Expression
genStr = fmap Str (sized genString)

genTime :: Int -> Gen Expression
genTime size = fmap Time (genExpression $ size `div` 2)

genIf :: Int -> Gen Expression
genIf size = liftM3 If (genExpression size')
                       (genExpression size')
                       (genExpression size')
  where size' = size `div` 2

genAnd :: Int -> Gen Expression
genAnd size = fmap And (listOf $ genExpression $ size `div` 2)

genOr :: Int -> Gen Expression
genOr size = fmap Or (listOf $ genExpression $ size `div` 2)

genCond :: Int -> Gen Expression
genCond size = liftM2 Cond (listOf $ genCondClause size') (genExpression size')
  where size' = size `div` 2

genCase :: Int -> Gen Expression
genCase size = liftM3 Case (genExpression size')
                           (listOf $ genCaseClause size')
                           (genExpression size')
  where size' = size `div` 2

varOfBinding :: LetBinding -> Variable
varOfBinding (LetBinding v _) = v

varOfRecBinding :: LetrecBinding -> Variable
varOfRecBinding (LetrecBinding v _) = v

genLet :: Int -> Gen Expression
genLet size =
    liftM2 Let (suchThat (listOf $ genLetBinding size')
                         (distinct . List.map varOfBinding))
               (genExpression size')
  where size' = size `div` 2

genLetstar :: Int -> Gen Expression
genLetstar size = liftM2 Letstar (listOf $ genLetBinding size')
                                 (genExpression size')
  where size' = size `div` 2

genLetrec :: Int -> Gen Expression
genLetrec size =
    liftM2 Letrec (suchThat (listOf $ genLetrecBinding size')
                            (distinct . List.map varOfRecBinding))
                  (genExpression size')
  where size' = size `div` 2

genBegin :: Int -> Gen Expression
genBegin size = liftM2 Begin (genExpression size') (genExpression size')
  where size' = size `div` 2

genUnless :: Int -> Gen Expression
genUnless size = liftM2 Unless (genExpression size') (genExpression size')
  where size' = size `div` 2

genQuote :: Int -> Gen Expression
genQuote size = fmap Quote (genQuotation size')
  where size' = size `div` 2

genLambda :: Int -> Gen LambdaAbstraction
genLambda size =
  liftM2 Lambda (suchThat (listOf $ genVariable size') distinct)
                (genExpression size')
  where size' = size `div` 2

genTraceLambda :: Int -> Gen LambdaAbstraction
genTraceLambda size =
  liftM3 TraceLambda (genVariable size')
                     (suchThat (listOf $ genVariable size') distinct)
                     (genExpression size')
  where size' = size `div` 2

genApplication :: Int -> Gen Expression
genApplication size = liftM2 Application (genExpression size')
                                         (genExpression size')
  where size' = size `div` 2

instance Arbitrary CondClause where
  arbitrary = sized genCondClause
  shrink = shrinkCondClause

genCondClause :: Int -> Gen CondClause
genCondClause size = oneof [ genImm size
                           , genGuarded size
                           , genGuardedImpl size ]

shrinkCondClause :: CondClause -> [CondClause]
shrinkCondClause (Immediate e) = [Immediate e' | e' <- shrink e]
shrinkCondClause (Guarded e1 e2) =
  [Guarded e1' e2' | e1' <- shrink e1
                  , e2' <- shrink e2 ]
shrinkCondClause (GuardedImpl e1 e2) =
  [GuardedImpl e1' e2' | e1' <- shrink e1
                       , e2' <- shrink e2 ]

instance Arbitrary CaseClause where
  arbitrary = sized genCaseClause
  shrink = shrinkCaseClause

genCaseClause :: Int -> Gen CaseClause
genCaseClause size = liftM2 CaseClause (listOf $ genQuotation size')
                                       (genExpression size')
  where size' = size `div` 2

instance Arbitrary LambdaAbstraction where
  arbitrary = sized genLambdaAbstraction
  shrink = shrinkLambdaAbstraction

shrinkCaseClause :: CaseClause -> [CaseClause]
shrinkCaseClause (CaseClause q e) =
  [CaseClause q' e' | q' <- shrink q
                    , e' <- shrink e ]

genImm :: Int -> Gen CondClause
genImm size = fmap Immediate (genExpression $ size `div` 2)

genGuarded :: Int -> Gen CondClause
genGuarded size = liftM2 Guarded (genExpression size') (genExpression size')
  where size' = size `div` 2

genGuardedImpl :: Int -> Gen CondClause
genGuardedImpl size = liftM2 GuardedImpl (genExpression size')
                                         (genExpression size')
  where size' = size `div` 2

instance Arbitrary LetBinding where
  arbitrary = sized genLetBinding
  shrink = shrinkLetbinding

genLetBinding :: Int -> Gen LetBinding
genLetBinding size = liftM2 LetBinding (genVariable size')
                                       (genExpression size')
  where size' = size `div` 2

instance Arbitrary LetrecBinding where
  arbitrary = sized genLetrecBinding
  shrink = shrinkLetrecBinding

genLetrecBinding :: Int -> Gen LetrecBinding
genLetrecBinding size = liftM2 LetrecBinding (genVariable size')
                                             (genLambdaAbstraction size')
  where size' = size `div` 2

shrinkLetrecBinding :: LetrecBinding -> [LetrecBinding]
shrinkLetrecBinding (LetrecBinding v lambda) =
  [LetrecBinding v lambda' | lambda' <- shrink lambda]

genLambdaAbstraction :: Int -> Gen LambdaAbstraction
genLambdaAbstraction size =
  oneof [genLambda size, genTraceLambda size]

shrinkLetbinding :: LetBinding -> [LetBinding]
shrinkLetbinding (LetBinding v e) = [LetBinding v' e' | v' <- shrink v
                                                      , e' <- shrink e ]

instance Arbitrary Quotation where
  arbitrary = sized genQuotation
  shrink = shrinkQuotation

genQuotation :: Int -> Gen Quotation
genQuotation size = oneof [ genQuoteNumber
                          , genQuoteBoolean
                          , genQuoteChar
                          , genQuoteString size
                          , genQuoteSymbol size
                          , genQuoteEmpty
                          , genQuotePair size]

genString :: Int -> Gen String
genString _ = do
  len <- choose (1, 5)
  vectorOf len (elements ['a'..'z'])

genQuoteNumber :: Gen Quotation
genQuoteNumber = fmap QuoteNumber arbitrary
genQuoteBoolean :: Gen Quotation
genQuoteBoolean = fmap QuoteBoolean arbitrary
genQuoteChar :: Gen Quotation
genQuoteChar = fmap QuoteChar (elements ['a'..'z'])
genQuoteString :: Int -> Gen Quotation
genQuoteString size = fmap QuoteString (genString $ size `div` 2)
genQuoteSymbol :: Int -> Gen Quotation
genQuoteSymbol size = fmap QuoteSymbol (genString $ size `div` 2)
genQuoteEmpty :: Gen Quotation
genQuoteEmpty = return QuoteEmpty
genQuotePair :: Int -> Gen Quotation
genQuotePair size = liftM2 QuotePair (genQuotation size')
                                     (genQuotation size')
  where size' = size `div` 2

shrinkQuotation :: Quotation -> [Quotation]
shrinkQuotation (QuoteNumber _) = [QuoteEmpty]
shrinkQuotation (QuoteBoolean _) = [QuoteEmpty]
shrinkQuotation (QuoteChar _) = [QuoteEmpty]
shrinkQuotation (QuoteString _) = [QuoteEmpty]
shrinkQuotation (QuoteSymbol _) = [QuoteEmpty]
shrinkQuotation QuoteEmpty = []
shrinkQuotation (QuotePair q1 q2) =
  QuoteEmpty : [QuotePair q1' q2' | q1' <- shrink q1
                                     , q2' <- shrink q2 ]

easy :: [Expression] -> [Expression]
easy es = [Number 0, Str "", Boolean True] ++ es

shrinkExpression :: Expression -> [Expression]
shrinkExpression (Number _) = []
shrinkExpression (Boolean _) = []
shrinkExpression (Ch _) = []
shrinkExpression (Str _) = []
shrinkExpression (Time e) = easy [e]
shrinkExpression (If e1 e2 e3) =
  easy [e1, e2, e3]
shrinkExpression (And es) =
  easy es ++ [And es' | es' <- shrink es]
shrinkExpression (Or es) =
    easy es ++ [Or es' | es' <- shrink es]
shrinkExpression (Cond clauses e) =
  easy [e] ++ [Cond clauses' e | clauses' <- shrink clauses]
shrinkExpression (Case e1 clauses e2) =
  easy [e1, e2] ++ [Case e1 clauses' e2 | clauses' <- shrink clauses]
shrinkExpression (Let bindings e) =
  easy [e] ++ [Let bindings' e | bindings' <- shrink bindings]
shrinkExpression (Letstar bindings e) =
  easy [e] ++ [Letstar bindings' e | bindings' <- shrink bindings]
shrinkExpression (Letrec bindings e) =
  easy [e] ++ [Letrec bindings' e | bindings' <- shrink bindings]
shrinkExpression (Begin e1 e2) =
  easy [e1, e2]
shrinkExpression (Unless e1 e2) =
  easy [e1, e2]
shrinkExpression (Quote q) = [Quote q' | q' <- shrink q]
shrinkExpression (LambdaExpression lambda) =
  List.map LambdaExpression (shrinkLambdaAbstraction lambda)
shrinkExpression (Application e1 e2) =
  easy [e1, e2] ++ [Application e1' e2' | e1' <- shrink e1
                                        , e2' <- shrink e2 ]

shrinkLambdaAbstraction :: LambdaAbstraction -> [LambdaAbstraction]
shrinkLambdaAbstraction (Lambda vs e) =
  [Lambda vs' e' | vs' <- shrink vs , e' <- shrink e]
shrinkLambdaAbstraction (TraceLambda v vs e) =
  Lambda vs e : [TraceLambda v vs' e' | vs' <- shrink vs
                                                                                             , e' <- shrink e ]
