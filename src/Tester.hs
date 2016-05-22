{- |
This script checkes the input programme against an output.

Remember that the text in between INPUT/OUPUT is EXACTLY compared, so pay
attention to the spaces/newlines.
-}

module Tester
  ( -- * Data types
    -- ** Input types
    TestName
  , TestInput
  , ExpectedOutput
  , TestAtom
    -- ** Output types
  , ActualOutput
  , TestResult(..)
  , TestResultAtom

    -- * Text formatting
    -- ** Input formatting
  , formatInStream
    -- ** Output formatting
  , displayResult
  , displayConclusion
    -- * Running tests
  , runTest
  ) where

import Control.Arrow (second)
import System.Exit (ExitCode(..))
import qualified Data.List.Split as L.Split (splitOn)
import qualified System.Process as P (shell, readCreateProcessWithExitCode)

-- Input types
type TestName = String
type TestInput = String
type ExpectedOutput = String
type TestAtom = (TestName, TestInput, ExpectedOutput)

-- Output types
type ActualOutput = String
data TestResult = RuntimeError String
                    -- ^ Test program exited with error message.
                | Fail ActualOutput ExpectedOutput
                    -- ^ Test program exited successfully, but result was
                    -- incorrect.
                | Pass
                    -- ^ All good.
  deriving (Show,Eq,Ord)
type TestResultAtom = (TestName, TestResult)

-- | Formats file input.
--
-- WARNING: Not pure function.
formatInStream :: String -> [TestAtom]
formatInStream rawIn =
  let -- First, split file for INPUT keyword.
    -- split1 :: [String] i.e. list of name/input/outputs
    split1 = tail . L.Split.splitOn "\nINPUT " $ ('\n':rawIn)
    -- Then, split the TestName from input/output. Drop delimiter.
    -- split2 :: [(TestName, String)]
    split2 = map (second tail . span (/='\n')) split1
    -- Third, filter for testname == empty or ends with '#'
    -- split3 :: [(TestName, String)]
    split3 = filter ((/='#') . last . ('#':) . fst) split2
    -- Fourth, split for OUTPUT keyword.
    -- split4 :: [(TestName, [ExpectedInput, ExpectedOutput])]
    split4 = map (second $ L.Split.splitOn "\nOUTPUT\n") split3
    -- Finally, rearrange into tuple.
  in map (\(a,[b,c]) -> (a,b,c)) split4

-- | Executes the given shell command, feeding the specified inputs to StdIn,
-- then generating the results.
runTest :: String -> TestAtom -> IO TestResultAtom
runTest cmdStr (testName, testIn, testOut) = do
  (rawExitCode, rawResult, rawError) <-
        P.readCreateProcessWithExitCode (P.shell cmdStr) testIn
  return $ ( testName
    , case rawExitCode of
        ExitSuccess | rawResult == testOut -> Pass
                    | otherwise            -> Fail rawResult testOut
        _                                  -> RuntimeError rawError
    )

-- | Formats result for display.
displayResult :: Int -> TestResultAtom -> String
displayResult i (testName, result) = "Test #" ++ show i ++ ": " ++
  case result of
    Pass -> "PASS [" ++ testName ++ "]"
    Fail response expected ->
      "FAIL [" ++ testName ++ "]\n--    Got: " ++
        show response ++ "\n-- Expect: " ++ show expected
    RuntimeError _ -> "ERROR [" ++ testName ++ "]"

-- | Calculates and displays overall test results.
displayConclusion :: [TestResult] -> String
displayConclusion testResultS =
  let overallResult = minimum testResultS
      resultSummaryText = case overallResult of
        Pass -> "PASS: "
        Fail _ _ -> "FAIL: "
        RuntimeError _ -> "ERROR: "
      numPass = length . filter (==Pass) $ testResultS
  in case overallResult of
    RuntimeError errMsg -> "-- First error message:\n" ++ errMsg ++ "\n"
    _ -> "" ++ resultSummaryText ++ show numPass
           ++ " / " ++ show (length testResultS) ++ " cases passed."
