
import System.Environment (getArgs)

import Data.Char (toLower)

import Happiness (HaStrategy (..))
import Solutions

main :: IO ()
main = do
  args <- getArgs
  (path, pnum, stgy) <- case args of
    path : pnum : stgy : _ -> (,,) path <$> readIO pnum <*> getStrategy stgy
    path : pnum : _ -> (,,) path <$> readIO pnum <*> pure Naive
    _               -> do
      putStr $ unlines
        [ "Usage: happiness FILENAME PROBLEM_ID"
        , "" ]
      fail "FILENAME and PROBLEM_ID required."

  h <- calcHappiness path pnum stgy
  putStrLn $ path ++ " " ++ show pnum ++ ": " ++ show h

getStrategy :: String -> IO HaStrategy
getStrategy s =
  maybe (fail $ "unknown happiness-strategy " ++ s) pure $
  lookup (map toLower s)
  [ ("naive", Naive)
  , ("wa", WeightedAverage)
  , ("weighted-avg", WeightedAverage)
  ]
