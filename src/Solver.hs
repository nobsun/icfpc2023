module Solver where

import Data.Aeson
import Data.ByteString.Lazy as B
import Problem
import Answer
import Happiness

class Solver a where
  apply :: a -> Problem -> Answer

solve :: Solver a => a -> Int -> IO ()
solve solver q = do
  problem <- readProblem q
  case problem of
    Nothing -> error $ "problem No. " ++ show q ++ " not found"
    Just p -> do
      let ans = solver `apply` p
      B.putStr $ encode ans
