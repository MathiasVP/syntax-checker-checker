module Main where

import QcSyntax
import CheckContains
import qualified Unparse
import qualified BuggyUnparse
import NumberOfNodes
import Test.QuickCheck
import Test.QuickCheck.Monadic
import System.Process
import System.Directory
import System.Environment
import System.IO
import qualified Data.List as List

setupCode :: FilePath -> String
setupCode path =
  unlines [ "(load \"" ++ path ++ "\")"
          , "(printf \"~s\" (check-program (read-file \"testcase.scm\")))" ]

lasts :: Int -> [a] -> [a]
lasts n xs = List.drop (List.length xs - n) xs


acc :: Accept
acc = CheckContains.acceptAll

restrict :: [String] -> Accept
restrict flags = List.foldr visit CheckContains.acceptAll flags
  where visit "toplevelform" accepts = accepts {topLevelForm = False}
        visit "toplevel-definition" accepts =
          accepts {topLevelDefinition = False}
        visit "toplevel-expression" accepts =
          accepts {topLevelExpression = False}
        visit "expression" accepts = accepts {expression = False}
        visit "number" accepts = accepts {number = False}
        visit "boolean" accepts = accepts {boolean = False}
        visit "char" accepts = accepts {ch = False}
        visit "string" accepts = accepts {str = False}
        visit "time" accepts = accepts {time = False}
        visit "if" accepts = accepts {if_ = False}
        visit "and" accepts = accepts {and_ = False}
        visit "or" accepts = accepts {or_ = False}
        visit "cond" accepts = accepts {cond = False}
        visit "case" accepts = accepts {case_ = False}
        visit "let" accepts = accepts {let_ = False}
        visit "let*" accepts = accepts {letstar = False}
        visit "letrec" accepts = accepts {letrec = False}
        visit "begin" accepts = accepts {begin = False}
        visit "unless" accepts = accepts {unless = False}
        visit "quote" accepts = accepts {quote = False}
        visit "application" accepts = accepts {application = False}
        visit "variable" accepts = accepts {variable = False}
        visit "cond-clause-immediate" accepts =
          accepts {condClauseImmediate = False}
        visit "cond-clause-guarded" accepts =
          accepts {condClauseGuarded = False}
        visit "cond-clause-guarded-impl" accepts =
          accepts {condClauseguardedImpl = False}
        visit "case-clause" accepts = accepts {caseClause = False}
        visit "let-binding" accepts = accepts {letBinding = False}
        visit "let*-binding" accepts = accepts {letstarBinding = False}
        visit "letrec-binding" accepts = accepts {letrecBinding = False}
        visit "trace-lambda" accepts = accepts {traceLambda = False}
        visit "lambda" accepts = accepts {lambda = False}
        visit "quote-number" accepts = accepts {quoteNumber = False}
        visit "quote-boolean" accepts = accepts {quoteBoolean = False}
        visit "quote-char" accepts = accepts {quoteChar = False}
        visit "quote-string" accepts = accepts {quoteString = False}
        visit "quote-symbol" accepts = accepts {quoteSymbol = False}
        visit "quote-empty" accepts = accepts {quoteEmpty = False}
        visit "quote-pair" accepts = accepts {quotePair = False}
        visit flag _ = error $ "Unregonized flag: " ++ flag



testValidSyntaxChecker :: Accept -> Program -> Int -> Int -> Property
testValidSyntaxChecker acc p _ _ =
  not (List.null p) && accepted acc p ==>
    monadicIO $ do
      result <- run $ do
        writeFile "testcase.scm" (Unparse.unparse p)
        putStr "."
        hFlush stdout
        readCreateProcess (shell "petite --script \"runtest.scm\"") ""
      assert $ lasts 2 result == "#t"

testInvalidSyntaxChecker :: Accept -> Program -> Int -> Int -> Property
testInvalidSyntaxChecker _ p point seed =
  point > 0 && point <= numberOfNodes p ==>
    monadicIO $ do
      result <- run $ do
        let p' = BuggyUnparse.unparse p point seed
        writeFile "testcase.scm" p'
        putStr "."
        hFlush stdout
        readCreateProcess (shell "petite --script \"runtest.scm\"") ""
      assert $ lasts 2 result == "#t"

usage :: String
usage = unlines ["Usage:\t\tpath/to/syntax/checker.scm pos|neg n [restrics]",
                 "\tpos:\t\tRun positive tests",
                 "\tneg:\t\tRun negative tests",
                 "\tn:\t\tNumber of tests",
                 "\tsize > 0:\tMaximum size for largest testcase",
                 "\t[restricts]:\tDo not generate these syntactic constructs.",
                 "\tThe possible values are:",
                 "\t\t-toplevelform",
                 "\t\t-toplevel-definition",
                 "\t\t-toplevel-expression",
                 "\t\t-expression",
                 "\t\t-number",
                 "\t\t-boolean",
                 "\t\t-char",
                 "\t\t-string",
                 "\t\t-time",
                 "\t\t-if",
                 "\t\t-and",
                 "\t\t-or",
                 "\t\t-cond",
                 "\t\t-case",
                 "\t\t-let",
                 "\t\t-let*",
                 "\t\t-letrec",
                 "\t\t-begin",
                 "\t\t-unless",
                 "\t\t-quote",
                 "\t\t-application",
                 "\t\t-variable",
                 "\t\t-cond-clause-immediate",
                 "\t\t-cond-clause-guarded",
                 "\t\t-cond-clause-guarded-impl",
                 "\t\t-case-clause",
                 "\t\t-let-binding",
                 "\t\t-let*-binding",
                 "\t\t-letrec-binding",
                 "\t\t-trace-lambda",
                 "\t\t-lambda",
                 "\t\t-quote-number",
                 "\t\t-quote-boolean",
                 "\t\t-quote-char",
                 "\t\t-quote-string",
                 "\t\t-quote-symbol",
                 "\t\t-quote-empty",
                 "\t\t-quote-pair"]

main :: IO ()
main = do
  args <- getArgs
  case args of
    path:b:n:maxsize:restricts -> do
      writeFile "runtest.scm" (setupCode path)
      let flags = restrict (List.map List.tail restricts)
          (qcheck, unparser, errormsg) =
            case b of
              "pos" -> (testValidSyntaxChecker flags,
                        \p _ _ -> Unparse.unparse p,
                        "Rejected valid program:")
              "neg" -> (testInvalidSyntaxChecker flags,
                        BuggyUnparse.unparse,
                        "Accepted invalid program:")
              _ -> error usage
      res <- quickCheckWithResult
               (stdArgs {chatty = False,
                         maxSuccess = read n,
                         maxDiscardRatio = 100000,
                         maxSize = read maxsize})
                        qcheck
      case res of
        Success {} -> putStrLn "OK"
        GaveUp _ _ msg  -> putStrLn msg
        Failure _ _ _ _ _ _ _ _ _ out -> do
          -- Ugly way of extracting the failing testcase
          let [p, point, seed] = lines $
                                 drop 2 $
                                 reverse $
                                 takeWhile (/= ':') $
                                 reverse out
              prog = unparser (read p) (read point) (read seed)
          putStrLn errormsg
          putStrLn prog
        _ -> error "An unknown error occured"
    _ -> error usage
