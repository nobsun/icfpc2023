module Solver where

import Data.Aeson
import Data.ByteString.Lazy as B
import Problem
import Answer
-- import Happiness

type Name = String
type SolverF = Problem -> IO (Either String [((Double, Double), Double)])

solve' :: Name
       -> SolverF
       -> Int
       -> IO (Answer, Problem)
solve' solverName solver probNum = do
  let probMark = "problem " ++ show probNum
  problem <- maybe (fail $ "parse error: " ++ probMark) pure =<< readProblem probNum
  res <- either (fail . (("solver error: " ++ probMark ++ ": solver " ++ solverName ++ ": ") ++)) pure =<< solver problem
  return (mkAnswer [ Placement x y | ((x, y), _) <- res ] [v | (_, v) <- res ], problem)

class Solver a where
  apply :: a -> Problem -> IO (Either String [((Double, Double), Double)])

solve :: Solver a => Name -> a -> Int -> IO ()
solve name solver pnum = B.putStr . encode . fst =<< solve' name (apply solver) pnum
