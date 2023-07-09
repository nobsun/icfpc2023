module Solver.NumericOptimization
  ( getCandidates
  ) where

import Control.Monad
import Data.Array.Unboxed
import Data.Either
import qualified Data.Vector as V
import qualified Data.Vector.Storable as VS
import Data.Vector.Storable (Vector)
import System.IO.Unsafe
import qualified System.Random.MWC as Rand
import qualified System.Random.MWC.Distributions as Rand
import qualified Numeric.Optimization as Opt

import Problem (Problem (..))
import qualified Problem
import Happiness hiding (happiness)
import Solver (SolverF)

import Debug.Trace


getCandidates :: SolverF
getCandidates problem = unsafePerformIO (getCandidatesIO problem)


getCandidatesIO :: Problem -> IO (Either String [(Double, Double)])
getCandidatesIO problem = do
  let numMusicians = length (Problem.musicians problem)
      numAttendees = length $ Problem.attendees problem
      numInstruments = length $ Problem.tastes $ Problem.attendees problem !! 0 

      tasteTable :: UArray (Int, Int) Double
      tasteTable = array ((0,0), (numAttendees-1, numInstruments-1)) $ do
        (i, attendee) <- zip [0..] (Problem.attendees problem)
        (j, like) <- zip [0..] (Problem.tastes attendee)
        return ((i,j), 1e6 * like)

      weightTable :: UArray (Int, Int) Double
      weightTable = array ((0,0), (numAttendees-1, numMusicians-1)) $ do
        i <- [0..numAttendees-1]
        k <- [0..numMusicians-1]
        return ((i,k), tasteTable ! (i, Problem.musicians problem !! k))

      squareDistance :: (Vector Double, Vector Double) -> UArray (Int, Int) Double
      squareDistance (xs, ys) = array ((0,0), (numAttendees-1, numMusicians-1)) $ do
        (i, Problem.Attendee{ Problem.x = x1, Problem.y = y1 }) <- zip [0..] (Problem.attendees problem)
        k <- [0..numMusicians-1]
        let x2 = xs VS.! k
            y2 = ys VS.! k
        return ((i,k), (x1-x2)^(2::Int) + (y1-y2)^(2::Int))

      squareDistanceBackward :: (Vector Double, Vector Double) -> UArray (Int, Int) Double -> (Vector Double, Vector Double)
      squareDistanceBackward (xs, ys) gret =
        ( VS.generate numMusicians $ \k ->
            let x2 = xs VS.! k
             in sum [(gret ! (i,k)) * 2 * (x1 - x2) * (-1)
                    | (i, Problem.Attendee{ Problem.x = x1 }) <- zip [0..] (Problem.attendees problem)
                    ]
        , VS.generate numMusicians $ \k ->
            let y2 = ys VS.! k
             in sum [(gret ! (i,k)) * 2 * (y1 - y2) * (-1)
                | (i, Problem.Attendee{ Problem.y = y1 }) <- zip [0..] (Problem.attendees problem)
                ]
        )

      -- ブロックされているかは考慮しないバージョン
      happyness :: (Vector Double, Vector Double) -> (Double, (Vector Double, Vector Double))
      happyness ps = (y, z)
        where
          d = squareDistance ps
          y = sum [(weightTable ! (i,k)) / (d ! (i,k)) | i <- [0..numAttendees-1], k <- [0..numMusicians-1]]
          z = squareDistanceBackward ps $ array (bounds weightTable) [(ik, (weightTable ! ik) * (-1) * (d ! ik)^^(-2::Int)) | ik <- indices weightTable]

      eps = 1e-6

      penalty :: (Vector Double, Vector Double) -> Double
      penalty (xs, ys) = sum
        [ 1 / (d2 / 100 + eps)
        | (k1, k2) <- pairs [0..numMusicians-1]
        , let d2 = ((xs VS.! k1) - (xs VS.! k2))^(2::Int) + ((ys VS.! k1) - (ys VS.! k2))^(2::Int)
        , d2 < 100
        ]

      penaltyBackward :: (Vector Double, Vector Double) -> Double -> (Vector Double, Vector Double)
      penaltyBackward (xs, ys) gret = 
        ( VS.replicate numMusicians 0 VS.// lefts us
        , VS.replicate numMusicians 0 VS.// rights us
        )
        where
          us = concat
            [ [ Left (k1, a * 2 * ((xs VS.! k1) - (xs VS.! k2)))
              , Left (k2, a * 2 * ((xs VS.! k1) - (xs VS.! k2)) * (-1))
              , Right (k1, a * 2 * ((ys VS.! k1) - (ys VS.! k2)))
              , Right (k2, a * 2 * ((ys VS.! k1) - (ys VS.! k2)) * (-1))
              ]
            | (k1, k2) <- pairs [0..numMusicians-1]
            , let d2 = ((xs VS.! k1) - (xs VS.! k2))^(2::Int) + ((ys VS.! k1) - (ys VS.! k2))^(2::Int)
            , d2 < 100
            , let a = gret * (-1) * (d2 / 100 + eps)^^(-2::Int) * (1/100)
            ]

      penaltyWeight :: Double
      penaltyWeight = 1e6

      f :: Vector Double -> Double
      f inp = traceShow (y1,y2,y) $ y
        where
          y = y1 + penaltyWeight * y2
          y1 = - fst (happyness ps)
          y2 = penalty ps
          ps = VS.splitAt numMusicians inp

      g :: Vector Double -> Vector Double
      g inp = VS.zipWith (+) (VS.map negate gxs) gxs2 <> VS.zipWith (+) (VS.map negate gys) gys2
        -- VS.map negate (gxs <> gys)
        -- gxs2 <> gys2
        where
          ps = VS.splitAt numMusicians inp
          (gxs, gys) = snd $ happyness ps
          (gxs2, gys2) = penaltyBackward ps penaltyWeight

  let (bx, by) = stage_bottom_left problem

      bound@((xl,xu),(yl,yu)) =
        ( (bx + 10, bx + stage_width problem - 10)
        , (by + 10, by + stage_height problem - 10)
        )

      bounds = V.fromList $ replicate numMusicians (fst bound) <> replicate numMusicians (snd bound)

  gen <- Rand.create
  xs0 <- replicateM numMusicians $ Rand.uniformR (xl, xu) gen
  ys0 <- replicateM numMusicians $ Rand.uniformR (yl, yu) gen
  let x0 = VS.fromList $ xs0 <> ys0

  let params :: Opt.Params (Vector Double)
      params = Opt.def
      -- params = def{ paramsMaxIters=Just 10 }
  result <- Opt.minimize Opt.LBFGSB params (f `Opt.WithGrad` g `Opt.WithBounds` bounds) x0

  if Opt.resultSuccess result || Opt.resultMessage result == "The number of steps exceeded the user's request." then do
    let (xs,ys) = VS.splitAt numMusicians (Opt.resultSolution result)
    return $ Right (zip (VS.toList xs) (VS.toList ys))
  else
    return $ Left (Opt.resultMessage result)


pairs :: [a] -> [(a,a)]
pairs [] = []
pairs (x:xs) = [(x,y) | y <- xs] ++ pairs xs


clamp :: Ord a => a -> a -> a -> a
clamp lo hi x
  | x < lo = lo
  | hi < x = hi
  | otherwise = x
