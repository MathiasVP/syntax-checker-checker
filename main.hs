module Main where

import QcSyntax
import qualified Unparse
import qualified BuggyUnparse
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

testSyntaxChecker :: (Program -> IO String) -> String -> Program -> Property
testSyntaxChecker printer expected p = not (List.null p) ==>
  monadicIO $ do
    result <- run $ do
      p' <- printer p
      writeFile "testcase.scm" p'
      putStr "."
      hFlush stdout
      readCreateProcess (shell "petite --script \"runtest.scm\"") ""
    assert $ lasts 2 result == expected

usage :: String
usage = "Usage: path/to/syntax/checker.scm 0|1"

main :: IO ()
main = do
  args <- getArgs
  case args of
    [path, b] -> do
      writeFile "runtest.scm" (setupCode path)
      let (unparse, expected, errormsg) =
            case b of
              "0" -> (return . Unparse.unparse, "#t", "Rejected valid program:")
              "1" -> (BuggyUnparse.unparse, "#f", "Accepted invalid program:")
              _ -> error usage
      res <- quickCheckWithResult
               (stdArgs {chatty = False})
               (testSyntaxChecker unparse expected)
      removeFile "testcase.scm"
      removeFile "runtest.scm"
      case res of
        Success {} -> putStrLn "OK"
        GaveUp {}  -> putStrLn "Timeout"
        Failure _ _ _ _ _ _ _ _ _ out -> do
          -- Ugly way of extracting the failing testcase
          let p = drop 3 $ dropWhile (/= ':') out
          putStrLn errormsg
          p' <- unparse $ read p
          putStrLn p'
        _ -> putStrLn "An unknown error occured"
    _ -> putStrLn usage
