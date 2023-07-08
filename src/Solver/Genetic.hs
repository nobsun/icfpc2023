module Solver.Genetic
  ( getCandidates
  ) where


import Data.List (sortBy, nub)
import Data.Maybe (catMaybes)
import Data.Function (on)
import System.IO.Unsafe (unsafePerformIO)
import System.Random (RandomGen, getStdGen, randomRs, randomRIO)

import Debug.Trace

import Problem (Problem (..))
import qualified Happiness hiding (happiness)
import Solver (SolverF)

type Stage = (Double,Double,Double,Double)
type Point = (Double,Double)

infixr 0 $$
f $$ x = traceShow x (f x)


getCandidates :: SolverF
getCandidates problem = unsafePerformIO (getCandidatesIO problem)

getCandidatesIO :: Problem -> IO (Either String [Point])
getCandidatesIO problem@(Problem{stage_width=w, stage_height=h ,stage_bottom_left=[zw,zh], musicians=ms, attendees=atnds}) = do
  g <- getStdGen
  let initials = take 2 $ repeat $ randomPlace g (w,h,zw,zh) (length ms)
  children <- go 2 initials
  return $ Right (head children)
  where
    go :: Double -> [[Point]] -> IO [[Point]]
    go size cands | size >= 16 = return cands
                  |otherwise   = do
      putStrLn $ "size: " ++ show size
      children <- genetic size cands
      go (size*2) (select 10 problem children)

    genetic :: Double -> [[Point]] -> IO [[Point]]
    genetic size cands = do
      g <- getStdGen
      let dw = w/size
          dh = h/size
          rs = [((dw*(i-1)+zw, dw*i+zw),(dh*(j-1)+zh, dh*i+zh)) |[i,j]<-splitBy 2 (randomRs (0,size-1) g)]
      return $ catMaybes[crossOver u v c d | ([c,d],(u,v))<-zip (sequence[cands,cands]) rs]


splitBy :: Int -> [a] -> [[a]]
splitBy n [] = []
splitBy n xs =
  let (as,bs) = splitAt n xs in as : splitBy n bs

select :: Int -> Problem -> [[Point]] -> [[Point]]
select num problem cands =
  take num $ map snd $ sortBy ((flip compare)`on`fst) [(happiness problem c, c) | c<-cands]


-- dummy for now
happiness :: Problem -> [Point] -> Double
happiness problem cands = 1

randomPlace :: RandomGen g => g -> Stage -> Int -> [Point]
randomPlace g (w,h,zw,zh) n = do
  randomPlace' n maxRetry [] randws randhs
  where
    maxRetry :: Int
    maxRetry = 20

    randws, randhs :: [Double]
    randws = [fromIntegral(r+zw')| r<-randomRs (10,truncate(w-10)::Int) g, let zw'=truncate zw]
    randhs = [fromIntegral(r+zh')| r<-randomRs (10,truncate(h-10)::Int) g, let zh'=truncate zh]

    randomPlace' :: Int -> Int -> [Point] -> [Double]-> [Double] -> [Point]
    randomPlace' 0 _ ps _ _ = ps
    randomPlace' k 0 ps _ _ = error "Can't place random."
    randomPlace' k r ps (px:rws) (py:rhs) = do
--      traceShow k (return ())
      if (or [ isMusicianConflict (px,py) (x,y) | (x,y)<-ps])
        then randomPlace' k (r-1) ps rws rhs
        else randomPlace'(k-1) maxRetry ((px,py):ps) rws rhs


isMusicianConflict :: Point -> Point -> Bool
isMusicianConflict (x1,y1) (x2,y2) =
  (x1-x2)^2 + (y1-y2)^2 <= 25


crossOver :: (Double,Double) -> (Double,Double) -> [Point] -> [Point] -> Maybe [Point]
crossOver (wl,wh) (hl,hh) as' bs' =
  case crossOver' [] (zip[1..]as') bs' of
    [] -> Nothing
    rs -> Just rs
  where
    musicians :: [Int]
    musicians = [i |(i,(x,y))<-zip[1..]bs', wl<=x,x<=wh, hl<=y,y<=hh]

    crossOver' :: [Point] -> [(Int,Point)] -> [Point] -> [Point]
    crossOver' rs [] _ = reverse rs
    crossOver' rs ((i,a):as) (b:bs) =
--      traceShow i $
      case (i`elem`musicians, or(map (isMusicianConflict b) rs)) of
        (False,_) -> crossOver' (a:rs) as bs
        (_,False) -> crossOver' (b:rs) as bs
        _         -> []


