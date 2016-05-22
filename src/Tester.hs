{- |
This script checkes the input programme against an output.

Remember that the text in between INPUT/OUPUT is EXACTLY compared, so pay
attention to the spaces/newlines.
-}

module Tester
  ( -- * ...
    formatInStream
  , runTest
  , displayResult
  , displayConclusion
  ) where

import Control.Arrow
import qualified Data.List.Split as L.Split (splitOn)
import qualified System.Process as P (readProcessWithExitCode)
import System.Exit (ExitCode(..))
import qualified System.Environment (getArgs)

type TestName = String
type TestInput = String
type ExpectedOutput = String
type TestAtom = (TestName, TestInput, ExpectedOutput)
type ActualOutput = String

data TestResult = RuntimeError String | Fail ActualOutput ExpectedOutput | Pass
    deriving (Show,Eq,Ord)

-- | WARN: Not pure function.
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

runTest :: FilePath -> FilePath -> TestAtom -> IO TestResult
runTest testCompilerFP testProgramFP (_,testIn,testOut) = do
    (rawExitCode,rawResult,rawError) <-
        P.readProcessWithExitCode testCompilerFP [testProgramFP] testIn
    return (if rawExitCode /= ExitSuccess then RuntimeError rawError
               else
                 if rawResult == testOut then Pass else Fail rawResult testOut)

displayResult :: (Int, TestName, TestResult) -> String
displayResult (i,testName,result) = "Test #" ++ show i ++ ": " ++
    case result of
        Pass -> "PASS [" ++ testName ++ "]"
        Fail response expected ->
            "FAIL [" ++ testName ++ "]\n--    Got: " ++
                show response ++ "\n-- Expect: " ++ show expected
        RuntimeError _ -> "ERROR [" ++ testName ++ "]"

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
            _ -> ""
        ++ resultSummaryText ++
            show numPass ++ " / " ++ show (length testResultS) ++ " cases passed."


main :: IO ()
main = do
    (testCompiler:testProgram:testCaseFile:_) <- System.Environment.getArgs
    testList <- formatInStream <$> readFile testCaseFile
    let testNameS = map (\(a,_,_)->a) testList
        -- resultIO :: [IO TestResult]
        resultIO = map (runTest testCompiler testProgram) testList
        -- formattedResultIO :: [IO (Int, TestName, TestResult)]
        formattedResultIO = map f . zip3 [(1::Int)..] testNameS $ resultIO
            where f (a,b,c) = c >>= (\x -> return (a,b,x))

    -- Run tests lazily and save result.
    testResultS <- mapM (\x -> do
                xDone@(_,_,testResult) <- x
                putStrLn . displayResult $ xDone
                return testResult
            ) formattedResultIO

    putStrLn ""
    putStrLn . displayConclusion $ testResultS
