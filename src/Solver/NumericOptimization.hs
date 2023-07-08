module Solver.NumericOptimization
  ( getCandidates
  ) where

import System.IO.Unsafe
import qualified System.Random.MWC as Rand
import qualified System.Random.MWC.Distributions as Rand
import Numeric.Optimization.AD

import Problem (Problem (..))
import qualified Problem
import Happiness hiding (happiness)
import Solver (SolverF)


getCandidates :: SolverF
getCandidates problem = unsafePerformIO (getCandidatesIO problem)


data P a = P [(a, a)]
  deriving (Functor, Foldable, Traversable, Show)

getCandidatesIO :: Problem -> IO (Either String [(Float, Float)])
getCandidatesIO problem = do
  let numMusicians = length (musicians problem)
      averageTaste = sum ts / fromIntegral (length ts)
        where
          ts = [abs (realToFrac like :: Double) | attendee <- Problem.attendees problem, like <- Problem.tastes attendee]

  let f :: (RealFrac a, Floating a, Show a) => P a -> a
      f x = happiness problem x + penalty x

      penalty :: (RealFrac a, Floating a) => P a -> a
      penalty (P ms) = sum
        [ realToFrac (1e6 * (10 * averageTaste) * 10^(2::Int)) / (d2 + eps)
        | ((x1,y1), (x2,y2)) <- pairs ms
        , let d2 = (x1 - x2)^(2::Int) + (y1 - y2)^(2::Int)
        ]
        where
          eps = 1e-6

      [bx, by] = stage_bottom_left problem

      bounds = P $ replicate numMusicians $
        ( (realToFrac (bx + 10), realToFrac (bx + stage_width problem - 10))
        , (realToFrac (by + 10), realToFrac (by + stage_height problem - 10))
        )

      x0_ = P $ replicate numMusicians
        ( realToFrac (bx + stage_width problem / 2)
        , realToFrac (bx + stage_height problem / 2)
        )

  gen <- Rand.create
  x0 <- mapM (\x -> (x+) <$> Rand.normal 0 1e-6 gen) x0_

  let params :: Params (P Double)
      params = def
      -- params = def{ paramsMaxIters=Just 10 }
  sol <- minimize LBFGSB params f (Just bounds) [] x0

  if resultSuccess sol then do
    let P ms = resultSolution sol
    return $ Right [(realToFrac x, realToFrac y) | (x,y) <- ms]
  else
    return $ Left (resultMessage sol)


-- | calculate happiness
happiness :: (RealFrac a, Floating a) => Problem -> P a -> a
happiness prob (P ms) = score
  where
    score = sum [ impact i k
                | k <- [0..length ms-1], i <- [0..length atnds-1], j <- [0..length ms-1]
                , i /= j
                , let Problem.Attendee{ Problem.x = ax, Problem.y = ay } = atnds !! i
                , not $ isBlock' (ms !! k) (realToFrac ax, realToFrac ay) (ms !! j)
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
