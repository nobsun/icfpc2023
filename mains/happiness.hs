
import System.Environment (getArgs)

import Data.Char (toLower)

import Happiness (HaStrategy (..))
import Solutions

main :: IO ()
main = do
  args <- getArgs
  (name, pnum, stgy) <- case args of
    name : pnum : stgy : _ -> (,,) name <$> readIO pnum <*> getStrategy stgy
    name : pnum : _ -> (,,) name <$> readIO pnum <*> pure Naive
    _               -> fail "NAME and PROBLEM_ID required."

  h <- calcHappiness name pnum stgy
  putStrLn $ name ++ " " ++ show pnum ++ ": " ++ show h

getStrategy :: String -> IO HaStrategy
getStrategy s =
  maybe (fail $ "unknown happiness-strategy " ++ s) pure $
  lookup (map toLower s)
  [ ("naive", Naive)
  , ("wa", WeightedAverage)
  , ("weighted-avg", WeightedAverage)
  ]
