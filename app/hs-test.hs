-- | hs-test app

module Main
  where

import Tester

import qualified System.Environment (getArgs)

getUserInput :: IO (FilePath, String)
getUserInput = do
  rArgs <- System.Environment.getArgs
  case rArgs of x:xs -> return $ (x, unwords xs)
                _ -> error "Use: hs-test test-file test-command"

main :: IO ()
main = do
  (testCaseFile, testCmd) <- getUserInput
  -- testList :: [TestAtom]
  testList <- formatInStream <$> readFile testCaseFile
  let -- resultIO :: [IO TestResultAtom]
      resultIO = map (runTest testCmd) testList

  -- Run tests lazily and save result.
  testResultS <- mapM (\(i, rIO) -> do
      rAt@(_, testResult) <- rIO
      putStrLn $ displayResult i rAt
      return testResult
    ) . zip [(1::Int)..] $ resultIO

  putStrLn ""
  putStrLn . displayConclusion $ testResultS
