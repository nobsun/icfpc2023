
import System.Environment (getArgs)

import Data.Char (toLower)
import Text.Printf (printf)
import System.Directory

import Happiness (HaStrategy (..))
import Solutions

main :: IO ()
main = do
  args <- getArgs
  (name, pnum, stgy) <- case args of
    name : pnum : stgy : _ -> (,,) name <$> readIO pnum <*> getStrategy stgy
    name : pnum : _ -> (,,) name <$> readIO pnum <*> pure Parallel
    _               -> do
      usage
      putStrLn ""
      fail "NAME and PROBLEM_ID required."

  isFileName <- doesFileExist name
  let path
        | isFileName  =  name
        | otherwise   =  printf "solutions/%s_%03d.json" name pnum

  h <- calcHappiness path pnum stgy
  putStrLn $ path ++ " " ++ show pnum ++ ": " ++ show h

usage :: IO ()
usage = do
  putStr $ unlines
    [ "Usage: happiness FILENAME PROBLEM_ID [STRATEGY_NAME]"
    , ""
    , "       happiness SOLVER_NAME PROBLEM_ID [STRATEGY_NAME]"
    , ""
    , "first check FILENAME, when not exist, try solusions/{SOLVER_NAME}_nnn.json"
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
