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


type Point = (Double,Double)

infixr 0 $$
f $$ x = traceShow x (f x)


getCandidates :: SolverF
getCandidates problem = unsafePerformIO (getCandidatesIO problem)

getCandidatesIO :: Problem -> IO (Either String [Point])
getCandidatesIO problem@(Problem{stage_width=w, stage_height=h ,stage_bottom_left=(zw,zh), musicians=ms}) = do
  g <- getStdGen
  -- generate initial placement randomly
  let initials = take 2 $ repeat $ randomPlace g problem [0..nMusician-1] []
  descendant <- go 2 initials
  -- descendant is sorted desc by happiness
  return $ Right (head descendant)
  where
    nMusician :: Int
    nMusician = length ms

    go :: Double -> [[Point]] -> IO [[Point]]
    go count cands | count >= 16 = return cands
                   | otherwise  = do
      putStrLn $ "count: " ++ show count
      -- create children
      children <- genetic count cands
      -- select top happiness group and go to the next generation
      go (count+2) (select 10 problem children)

    genetic :: Double -> [[Point]] -> IO [[Point]]
    genetic count cands = do
      g <- getStdGen
      let dw = w/count
          dh = h/count
          rs = [((dw*(i-1)+zw, dw*i+zw),(dh*(j-1)+zh, dh*i+zh)) |[i,j]<-splitBy 2 (randomRs (0,count-1) g)]
      return $ catMaybes
        -- crossover c and d by area
        $ [crossOver u v c d | ([c,d],(u,v))<-zip (sequence[cands,cands]) rs]
        -- replace n musicians randomly
        ++[mutation g problem n c | let n=nMusician`div`(truncate count), c<-cands]


splitBy :: Int -> [a] -> [[a]]
splitBy n [] = []
splitBy n xs =
  let (as,bs) = splitAt n xs in as : splitBy n bs

-- select top num happiness group
select :: Int -> Problem -> [[Point]] -> [[Point]]
select num problem cands =
  take num $ map snd $ sortBy ((flip compare)`on`fst) [(happiness problem c, c) | c<-cands]


-- dummy for now
happiness :: Problem -> [Point] -> Double
happiness problem cands = 1

-- randomly add musicians(targets) to the given placement(initial).
randomPlace :: RandomGen g => g -> Problem -> [Int] -> [(Int,Point)] -> [Point]
randomPlace g (Problem{stage_width=w, stage_height=h ,stage_bottom_left=(zw,zh)}) targets initial = do
  map snd $ sortBy(compare`on`fst) $ randomPlace' targets maxRetry initial randws randhs
  where
    maxRetry :: Int
    maxRetry = 20

    randws, randhs :: [Double]
    randws = [fromIntegral(r+zw')| r<-randomRs (10,truncate(w-10)::Int) g, let zw'=truncate zw]
    randhs = [fromIntegral(r+zh')| r<-randomRs (10,truncate(h-10)::Int) g, let zh'=truncate zh]

    randomPlace' :: [Int] -> Int -> [(Int,Point)] -> [Double]-> [Double] -> [(Int,Point)]
    randomPlace' []     _ ps _ _ = ps
    randomPlace' _      0 _  _ _ = error "Can't place random."
    randomPlace' (k:ks) r ps (kx:kxs) (ky:kys) = do
--      traceShow k (return ())
      if (or [ isMusicianConflict (kx,ky) (x,y) | (_,(x,y))<-ps])
        then randomPlace' (k:ks) (r-1) ps kxs kys --retry
        else randomPlace' ks maxRetry ((k,(kx,ky)):ps) kxs kys


isMusicianConflict :: Point -> Point -> Bool
isMusicianConflict (x1,y1) (x2,y2) =
  (x1-x2)^2 + (y1-y2)^2 <= 25

-- embed bs' area(wl,wh,hl,hh) to as
crossOver :: (Double,Double) -> (Double,Double) -> [Point] -> [Point] -> Maybe [Point]
crossOver (wl,wh) (hl,hh) as' bs' =
  case crossOver' [] (zip[0..]as') bs' of
    [] -> Nothing
    rs -> Just rs
  where
    musicians :: [Int]
    musicians = [i |(i,(x,y))<-zip[0..]bs', wl<=x,x<=wh, hl<=y,y<=hh]

    crossOver' :: [Point] -> [(Int,Point)] -> [Point] -> [Point]
    crossOver' rs [] _ = reverse rs
    crossOver' rs ((i,a):as) (b:bs) =
--      traceShow i $
      case (i`elem`musicians, or(map (isMusicianConflict b) rs)) of
        (False,_) -> crossOver' (a:rs) as bs
        (_,False) -> crossOver' (b:rs) as bs
        _         -> []

-- replace n musicians in as randomly
mutation :: RandomGen g => g -> Problem-> Int -> [Point] -> Maybe [Point]
mutation g problem@(Problem{musicians=ms}) n as =
  Just $ randomPlace g problem targets [(i,a)|(i,a)<-zip[0..]as, i`notElem`targets]
  where
    targets = nub $ take n $ randomRs (0,length ms-1) g
