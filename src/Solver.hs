module Solver where

import Data.Aeson
import Data.ByteString.Lazy as B
import Problem
import Answer
import Happiness

type Name = String
type SolverF = Problem -> Either String [(Float, Float)]

solve' :: Name
       -> SolverF
       -> Int
       -> IO Answer
solve' solverName solver probNum = do
  let probMark = "problem " ++ show probNum
  problem <- maybe (fail $ "parse error: " ++ probMark) pure =<< readProblem probNum
  cords <- either (fail . (("solver error: " ++ probMark ++ ": solver " ++ solverName ++ ": ") ++)) pure $ solver problem
  return $ Answer { placements = [ Placement x y | (x, y) <- cords ] }

class Solver a where
  apply :: a -> Problem -> Either String [(Float, Float)]

solve :: Solver a => Name -> a -> Int -> IO ()
solve name solver pnum = B.putStr . encode =<< solve' name (apply solver) pnum
