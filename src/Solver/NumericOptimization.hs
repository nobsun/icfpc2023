module Solver.NumericOptimization
  ( getCandidates
  ) where

import Control.Monad
import System.IO.Unsafe
import qualified System.Random.MWC as Rand
import qualified System.Random.MWC.Distributions as Rand
import Numeric.Optimization.AD

import Problem (Problem (..))
import qualified Problem
import Happiness hiding (happiness)
import Solver (SolverF)

import Debug.Trace


getCandidates :: SolverF
getCandidates problem = unsafePerformIO (getCandidatesIO problem)


data P a = P [(a, a)]
  deriving (Functor, Foldable, Traversable, Show)

getCandidatesIO :: Problem -> IO (Either String [(Double, Double)])
getCandidatesIO problem = do
  let numMusicians = length (musicians problem)

      weight :: Double
      weight = 1e6 * sum [abs like | attendee <- Problem.attendees problem, like <- Problem.tastes attendee]

  let f :: (RealFrac a, Floating a, Show a) => P a -> a
      f x = traceShow (y1,y2,y) $ y
        where
          y = y1 + y2
          y1 = - happiness problem x
          y2 = realToFrac weight * penalty x

      penalty :: (RealFrac a, Floating a) => P a -> a
      penalty (P ms) = sum
        [ 1 / (d2 / 100 + eps)
        | ((x1,y1), (x2,y2)) <- pairs ms
        , let d2 = (x1 - x2)^(2::Int) + (y1 - y2)^(2::Int)
        , d2 < 100
        ]
        where
          eps = 1e-6

      (bx, by) = stage_bottom_left problem

      bound@((xl,xu),(yl,yu)) =
        ( (bx + 10, bx + stage_width problem - 10)
        , (by + 10, by + stage_height problem - 10)
        )

      bounds = P $ replicate numMusicians bound

  gen <- Rand.create
  x0_ <- replicateM numMusicians $ do
{-
    let center_x = bx + stage_width problem / 2
        center_y = by + stage_height problem / 2
    dx <- Rand.normal 0 1e-1 gen
    dy <- Rand.normal 0 1e-1 gen
    return (clamp xl xu (center_x + dx), clamp yl yu (center_y + dy))
-}
    x <- Rand.uniformR (xl, xu) gen
    y <- Rand.uniformR (yl, yu) gen
    return (x, y)
  let x0 = P x0_

  let params :: Params (P Double)
      params = def
      -- params = def{ paramsMaxIters=Just 10 }
  result <- minimize LBFGSB params f (Just bounds) [] x0

  if resultSuccess result || resultMessage result == "The number of steps exceeded the user's request." then do
    let P ms = resultSolution result
    return $ Right ms
  else
    return $ Left (resultMessage result)


-- | calculate happiness
happiness :: (RealFrac a, Floating a) => Problem -> P a -> a
happiness prob (P ms) = score
  where
    score = sum [ impact i k
                | k <- [0..length ms-1], i <- [0..length atnds-1]
                , let Problem.Attendee{ Problem.x = ax, Problem.y = ay } = atnds !! i
                , and [not $ isBlock' (ms !! k) (realToFrac ax, realToFrac ay) (ms !! j) | j <- [0..length ms-1], k /= j]
                ]
    atnds = attendees prob
    squareDistance (x1, y1) (x2, y2) = (x1 - x2)^(2::Int) + (y1 - y2)^(2::Int)
    impact i k = num / den
      where
        num = realToFrac $ 1e6 * (Problem.tastes (atnds !! i) !! (musicians prob !! k))
        den = squareDistance (ms !! k) (realToFrac ax, realToFrac ay)
        Problem.Attendee{ Problem.x = ax, Problem.y = ay } = atnds !! i


pairs :: [a] -> [(a,a)]
pairs [] = []
pairs (x:xs) = [(x,y) | y <- xs] ++ pairs xs


clamp :: Ord a => a -> a -> a -> a
clamp lo hi x
  | x < lo = lo
  | hi < x = hi
  | otherwise = x
