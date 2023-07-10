module Solver.NumericOptimization
  ( getCandidates
  ) where

import Control.Monad
import Data.Array.IArray
import Data.Array.Unboxed
import Data.Either
import qualified Data.Vector as V
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Generic as VG
import Data.Vector.Storable (Vector)
import System.IO.Unsafe
import qualified System.Random.MWC as Rand
import qualified System.Random.MWC.Distributions as Rand
import Text.Printf
import qualified Numeric.Optimization as Opt
import Numeric.AD

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
        return ((i,j), like)

      weightTable :: UArray (Int, Int) Double
      weightTable = array ((0,0), (numAttendees-1, numMusicians-1)) $ do
        i <- [0..numAttendees-1]
        k <- [0..numMusicians-1]
        return ((i,k), tasteTable ! (i, Problem.musicians problem !! k))

      squareDistance :: (VG.Vector v e, IArray a e, Fractional e) => (v e, v e) -> a (Int, Int) e
      squareDistance (xs, ys) = array ((0,0), (numAttendees-1, numMusicians-1)) $ do
        (i, Problem.Attendee{ Problem.x = x1, Problem.y = y1 }) <- zip [0..] (Problem.attendees problem)
        k <- [0..numMusicians-1]
        let x2 = xs VG.! k
            y2 = ys VG.! k
        return ((i,k), (realToFrac x1 - x2)^(2::Int) + (realToFrac y1 - y2)^(2::Int))

      squareDistanceBackward :: (Vector Double, Vector Double) -> UArray (Int, Int) Double -> (Vector Double, Vector Double)
      squareDistanceBackward (xs, ys) gret =
        ( VS.generate numMusicians $ \k ->
            let x2 = xs VS.! k
             in sum [ (gret ! (i,k)) * 2 * (x1 - x2) * (-1)
                    | (i, Problem.Attendee{ Problem.x = x1 }) <- zip [0..] (Problem.attendees problem)
                    ]
        , VS.generate numMusicians $ \k ->
            let y2 = ys VS.! k
             in sum [ (gret ! (i,k)) * 2 * (y1 - y2) * (-1)
                    | (i, Problem.Attendee{ Problem.y = y1 }) <- zip [0..] (Problem.attendees problem)
                    ]
        )

      -- ブロックされているかは考慮しないバージョンのhappinessとその勾配を計算
      -- 数値が大きくなりすぎると計算が不安定になるかもと思って 1e6 をかけない状態での値で扱っている
      happyness :: (Vector Double, Vector Double) -> (Double, (Vector Double, Vector Double))
      happyness ps = (y, z)
        where
          d :: UArray (Int,Int) Double
          d = squareDistance ps
          y = sum [(weightTable ! ik) / (d ! ik) | ik <- indices weightTable]
          z = squareDistanceBackward ps $ array (bounds weightTable) [(ik, (weightTable ! ik) * (-1) * (d ! ik)^^(-2::Int)) | ik <- indices weightTable]

      -- デバッグ用
      happynessAD :: (Vector Double, Vector Double) -> (Double, (Vector Double, Vector Double))
      happynessAD (xs, ys) =
        case grad' f (VG.convert (xs <> ys) :: V.Vector Double) of
          (val, xs'_ys') -> (val, VG.splitAt numMusicians $ VG.convert xs'_ys')
        where
          f :: forall e. Fractional e => V.Vector e -> e
          f xs_ys = y
            where
              ps = V.splitAt numMusicians xs_ys
              d :: Array (Int,Int) e
              d = squareDistance ps
              y = sum [realToFrac (weightTable ! ik) / (d ! ik) | ik <- indices weightTable]

      eps :: RealFrac e => e
      eps = 1e-6

      penalty :: (VG.Vector v e, RealFrac e) => (v e, v e) -> e
      penalty (xs, ys) = sum
        [ 1 / (d2 / 100 + eps)
        | (k1, k2) <- pairs [0..numMusicians-1]
        , let (x1, y1) = (xs VG.! k1, ys VG.! k1)
        , let (x2, y2) = (xs VG.! k2, ys VG.! k2)
        , let d2 = (x1 - x2)^(2::Int) + (y1 - y2)^(2::Int)
        , d2 < 100
        ]

      penaltyBackward :: (Vector Double, Vector Double) -> Double -> (Vector Double, Vector Double)
      penaltyBackward (xs, ys) gret = 
        ( VS.accum (+) (VS.replicate numMusicians 0) (lefts us)
        , VS.accum (+) (VS.replicate numMusicians 0) (rights us)
        )
        where
          us = concat
            [ [ Left (k1, a * 2 * (x1 - x2))
              , Left (k2, a * 2 * (x1 - x2) * (-1))
              , Right (k1, a * 2 * (y1 - y2))
              , Right (k2, a * 2 * (y1 - y2) * (-1))
              ]
            | (k1, k2) <- pairs [0..numMusicians-1]
            , let (x1, y1) = (xs VG.! k1, ys VG.! k1)
            , let (x2, y2) = (xs VG.! k2, ys VG.! k2)
            , let d2 = (x1 - x2)^(2::Int) + (y1 - y2)^(2::Int)
            , d2 < 100
            , let a = gret * (-1) * (d2 / 100 + eps)^^(-2::Int) * (1/100)
            ]

      -- デバッグ用
      penaltyBackwardAD :: (Vector Double, Vector Double) -> Double -> (Vector Double, Vector Double)
      penaltyBackwardAD (xs, ys) gret
        = VG.splitAt numMusicians $ VG.map (gret *) $ VG.convert
        $ grad (\xs_ys -> penalty (VG.splitAt numMusicians xs_ys))
        $ (VG.convert (xs <> ys) :: V.Vector Double)

      penaltyWeight :: Double
      penaltyWeight = 10

      -- 数値が大きくなりすぎると計算が不安定になるかもと思って 1e6 をかけない状態での値を目的関数にしている
      -- ceiling をしないのでスケールは関係ない
      f :: Vector Double -> Double
      f inp = trace (printf "obj = %f = %f + %f; approximate happiness = %f; penalty = %f" obj obj1 (penaltyWeight * obj2) (h * 1e6) obj2) $ obj
        where
          obj = obj1 + penaltyWeight * obj2
          h = fst (happyness ps)
          obj1 = - h
          obj2 = penalty ps
          ps = VS.splitAt numMusicians inp

      g :: Vector Double -> Vector Double
      g inp = VS.zipWith (+) (VS.map negate gxs) gxs2 <> VS.zipWith (+) (VS.map negate gys) gys2
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

  -- メッセージのスペースに注意
  if Opt.resultSuccess result then do
    let (xs,ys) = VS.splitAt numMusicians (Opt.resultSolution result)
    return $ Right (zip (VS.toList xs) (VS.toList ys))
  else if Opt.resultMessage result `elem` ["The number of steps exceeded the user's request.", "ABNORMAL_TERMINATION_IN_LNSRCH                              "] then do
    traceM (Opt.resultMessage result)
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
