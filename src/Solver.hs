module Solver where

import Data.Aeson
import Data.ByteString.Lazy as B
import Problem
import Answer
-- import Happiness

type Name = String
type SolverF = Problem -> Either String [(Double, Double)]

solve' :: Name
       -> SolverF
       -> Int
       -> IO (Answer, Problem)
solve' solverName solver probNum = do
  let probMark = "problem " ++ show probNum
  problem <- maybe (fail $ "parse error: " ++ probMark) pure =<< readProblem probNum
  cords <- either (fail . (("solver error: " ++ probMark ++ ": solver " ++ solverName ++ ": ") ++)) pure $ solver problem
  return (Answer { placements = [ Placement x y | (x, y) <- cords ] }, problem)

class Solver a where
  apply :: a -> Problem -> Either String [(Double, Double)]

solve :: Solver a => Name -> a -> Int -> IO ()
solve name solver pnum = B.putStr . encode . fst =<< solve' name (apply solver) pnum
