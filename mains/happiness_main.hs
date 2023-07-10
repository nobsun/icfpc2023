
import System.Environment (getArgs)

import Data.Char (toLower)
import Text.Printf (printf)
import System.Directory

import Happiness (HaStrategy (..))
import Solutions

main :: IO ()
main = do
  args <- getArgs
  (name, may_pnum, stgy) <- case args of
    name : pnum : stgy : _ -> (,,) name <$> (Just <$> readIO pnum) <*> getStrategy stgy
    name : pnum : [] -> (,,) name <$> (Just <$> readIO pnum) <*> pure Parallel
    name : []        -> pure (name, Nothing, Parallel)
    _               ->  usageAndFail "FILENAME or SOLVER_NAME and PROBLEM_ID required."

  isFileName <- doesFileExist name
  (path, pnum) <- case may_pnum of
    Just pnum
      | isFileName       -> pure (name, pnum)
      | otherwise        -> pure (printf "solutions/%s_%03d.json" name pnum, pnum)
    Nothing
      | isFileName ->  case fromFilenameHeulistic name of
          Just (_, pnum) -> pure (name, pnum)
          Nothing        -> usageAndFail "Fail to find PROBLEM_ID with heuristics."
    _                    -> usageAndFail "FILENAME or SOLVER_NAME and PROBLEM_ID required."

  h <- calcHappiness path pnum stgy
  putStrLn $ path ++ " " ++ show pnum ++ ": " ++ show h

usageAndFail :: String -> IO a
usageAndFail s = do
  usage
  putStrLn ""
  fail s

usage :: IO ()
usage = do
  putStr $ unlines
    [ "Usage: happiness FILENAME [PROBLEM_ID] [STRATEGY_NAME]"
    , ""
    , "       happiness SOLVER_NAME PROBLEM_ID [STRATEGY_NAME]"
    , ""
    , "first check FILENAME, when not exist, try solusions/{SOLVER_NAME}_nnn.json."
    , "when PROBLEM_ID is not passed, calculating it with heuristics."
    , ""
    , "supported strategies:"
    , "  para - Parallel strategy (Naive with parallel execution)"
    , "  naive - Naive strategy"
    , "  wa | weighted-avg - WeightedAverage strategy"
    ]

getStrategy :: String -> IO HaStrategy
getStrategy s =
  maybe (fail $ "unknown happiness-strategy " ++ s) pure $
  lookup (map toLower s)
  [ ("naive", Naive)
  , ("para", Parallel)
  , ("wa", WeightedAverage)
  , ("weighted-avg", WeightedAverage)
  ]
