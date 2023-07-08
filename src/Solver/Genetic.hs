module Solver.Genetic
  ( getCandidates
  ) where


import Data.List (sortBy, nub, partition)
import Data.Maybe (catMaybes)
import Data.Function (on)
import System.IO.Unsafe (unsafePerformIO)
import System.Random (RandomGen, newStdGen, randomRs, randomRIO)

import Debug.Trace

import Problem (Problem(..), Attendee(..))
import Answer (Answer(..), Placement(..))
import Happiness (Happiness, weightedAverageHappiness)
import Solver (SolverF)


type Point = (Double,Double)

infixr 0 $$
f $$ x = traceShow x (f x)


getCandidates :: SolverF
getCandidates problem = unsafePerformIO (getCandidatesIO problem)

getCandidatesIO :: Problem -> IO (Either String [Point])
getCandidatesIO problem@(Problem{stage_width=w, stage_height=h ,stage_bottom_left=(zw,zh), musicians=ms}) = do
  putStrLn $ "stage-width: "++show w++", stage_height: "++show h++", musicians: "++show nMusician
  g <- newStdGen
  putStrLn $ "generating initial placement randomly"
  initials <- sequence $ take 2 $ repeat $ randomPlace problem (zip[0..nMusician-1](repeat undefined)) []
  print (initials!!0 == initials!!1)
  putStrLn $ "happiness: "++ show[happiness problem i | i<-initials]
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
      putStrLn "creating children..."
      children <- genetic count cands
      let hchildren = sortBy (flip compare`on`fst)[(happiness problem c, c) | c<-children]
      putStrLn $ "happiness: "++show (map fst hchildren)
      go (count+2) (map snd $ take 2 hchildren)

    genetic :: Double -> [[Point]] -> IO [[Point]]
    genetic count cands = do
      g <- newStdGen
      let dw = w/count
          dh = h/count
          rs = [((dw*(i-1)+zw, dw*i+zw),(dh*(j-1)+zh, dh*i+zh)) |[i,j]<-splitBy 2 (randomRs (0,count-1) g)]
      -- crossover c and d by area
      a1 <- sequence [crossOver u v c d | ([c,d],(u,v))<-zip (sequence[cands,cands]) rs]
      -- replace n musicians randomly
      a2 <- sequence [mutation problem n c | let n=nMusician`div`(truncate count), c<-cands]
      return $ catMaybes (a1++a2)


splitBy :: Int -> [a] -> [[a]]
splitBy n [] = []
splitBy n xs =
  let (as,bs) = splitAt n xs in as : splitBy n bs

happiness :: Problem -> [Point] -> Happiness
happiness problem cands =
  weightedAverageHappiness problem answer
  where
    answer = Answer{placements=[Placement x y |(x,y)<-cands]}

-- randomly add musicians(targets) to the given placement(initial).
randomPlace :: Problem -> [(Int,Point)] -> [(Int,Point)] -> IO [Point]
randomPlace (Problem{stage_width=w, stage_height=h ,stage_bottom_left=(zw,zh)}) targets initial = do
  g <- newStdGen
  let randws = [fromIntegral(r+zw')| r<-randomRs (10,truncate(w-10)::Int) g, let zw'=truncate zw]
      randhs = [fromIntegral(r+zh')| r<-randomRs (10,truncate(h-10)::Int) g, let zh'=truncate zh]
  return $ map snd $ sortBy(compare`on`fst) $ randomPlace' targets maxRetry initial randws randhs
  where
    maxRetry :: Int
    maxRetry = 20

    randomPlace' :: [(Int,Point)] -> Int -> [(Int,Point)] -> [Double]-> [Double] -> [(Int,Point)]
    randomPlace' []               _ ps _   _   = ps
    randomPlace' ((k,current):ks) 0 ps kxs kys = randomPlace' ks maxRetry ((k,current):ps) kxs kys
    randomPlace' ((k,current):ks) r ps (kx:kxs) (ky:kys) = do
--      traceShow k (return ())
      if (or [ isMusicianConflict (kx,ky) (x,y) | (_,(x,y))<-ps])
        then randomPlace' ((k,current):ks) (r-1) ps kxs kys --retry
        else randomPlace' ks maxRetry ((k,(kx,ky)):ps) kxs kys


isMusicianConflict :: Point -> Point -> Bool
isMusicianConflict (x1,y1) (x2,y2) =
  (x1-x2)^2 + (y1-y2)^2 <= 25

-- embed bs' area(wl,wh,hl,hh) to as
crossOver :: (Double,Double) -> (Double,Double) -> [Point] -> [Point] -> IO (Maybe [Point])
crossOver (wl,wh) (hl,hh) as' bs' =
  return $ case crossOver' [] (zip[0..]as') bs' of
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
mutation :: Problem-> Int -> [Point] -> IO (Maybe [Point])
mutation problem@(Problem{musicians=ms}) n as = do
  g <- newStdGen
  let (rest,target) = partition (\(i,a)->i`elem`targets) (zip[0..]as)
      targets = nub $ take n $ randomRs (0,length ms-1) g
  fmap Just $ randomPlace problem target rest
