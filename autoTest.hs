{-# OPTIONS_GHC -Wall                    #-}
--{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

--import Data.Ord
--import Data.Function
--import Data.Monoid
import Control.Applicative
--import Control.Monad
import Control.Arrow
--import qualified Control.Monad.State as ST
--import qualified Data.Char as C
--import Data.List
import qualified Data.List.Split as L.Split
--import qualified Data.Set as Set
--import qualified Data.Map.Strict as Map
--import qualified Data.Vector as V
--import qualified Data.Vector.Unboxed as VU
--import qualified Data.Vector.Generic as VG
--import qualified Data.Foldable as F
--import qualified Data.Maybe
--import qualified Safe
--import Control.DeepSeq

import qualified System.Process as P
import System.Exit (ExitCode(..))
import qualified System.Environment (getArgs)

-- | This script checkes the input programme against an output.
--   Remember that the text inbetween INPUT/OUPUT is EXACTLY compared, so pay
--   attention to the spaces/newlines.

type TestName = String
type TestInput = String
type ExpectedOutput = String
type TestAtom = (TestName, TestInput, ExpectedOutput)

data TestResult = RuntimeError String | Fail String ExpectedOutput | Pass
    deriving (Eq,Show)

-- | WARN: Not pure function.
formatInStream :: String -> [TestAtom]
formatInStream rawIn =
    let -- First, split file for INPUT keyword.
        -- split1 :: [String] i.e. list of name/input/outputs
        split1 = tail . L.Split.splitOn "\nINPUT " $ ('\n':rawIn)
        -- Then, split the TestName from input/output. Drop delimiter.
        -- split2 :: [(TestName, String)]
        split2 = map (second tail . span (/='\n')) split1
        -- Third, split for OUTPUT keyword.
        -- split3 :: [(TestName, [ExpectedInput, ExpectedOutput])]
        split3 = map (second $ L.Split.splitOn "\nOUTPUT\n") split2
        -- Finally, rearrange into tuple.
    in map (\(a,[b,c]) -> (a,b,c)) split3

checkInputOutput :: FilePath -> TestAtom -> IO TestResult
checkInputOutput fileP (_,testIn,testOut) = do
    (rawExitCode,rawResult,rawError) <- P.readProcessWithExitCode "python" [fileP] testIn
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
        RuntimeError errMsg ->
            "ERROR [" ++ testName ++ "]\n-- Message: " ++ errMsg

displayConclusion :: [TestResult] -> String
displayConclusion testResultS =
    let overallPass = all (==Pass) testResultS
        numPass = length . filter (==Pass) $ testResultS
    in (if overallPass then "PASS: " else "FAIL: ") ++
            show numPass ++ " / " ++ show (length testResultS) ++ " cases passed."

main :: IO ()
main = do
    testName <- System.Environment.getArgs
    testList <- formatInStream <$> readFile (head testName ++ ".test")
    result <- mapM (checkInputOutput $ head testName) testList
    let testNameS = map (\(a,_,_)->a) testList
    mapM_ (putStrLn . displayResult) $ zip3 [(1::Int)..] testNameS result
    putStrLn ""
    putStrLn $ displayConclusion result
