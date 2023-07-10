{-# language TupleSections #-}

module Solver.Genetic
  ( getCandidatesIO
  ) where


import Data.List (sortBy, nub, partition)
import Data.Maybe (catMaybes)
import Data.Function (on)
import System.IO.Unsafe (unsafePerformIO)
import System.Random (RandomGen, newStdGen, randomRs, randomRIO)

import Debug.Trace

import Problem (Problem(..), Attendee(..))
import Answer (Answer(..), mkAnswer, Placement(..))
import Extra
import Happiness (Happiness, memoise)
import Solver (SolverF)


type Point = (Double,Double)
type Volume = Double

infixr 0 $$
($$) :: Show a => (a -> b) -> a -> b
f $$ x = traceShow x (f x)


getCandidatesIO :: SolverF
getCandidatesIO problem@(Problem{stage_width=w, stage_height=h ,stage_bottom_left=(zw,zh), musicians=ms}) = do
  putStrLn $ "stage-width: "++show w++", stage_height: "++show h++", musicians: "++show nMusician
  putStrLn $ "generating initial placement randomly"
  initials <- sequence $ take 2 $ repeat $ randomPlace problem (zip[0..nMusician-1](repeat undefined)) []
  extra <- mkExtra problem (toAnswer (initials!!0))
  initHappiness <- sequence[happiness extra problem i curNo 0 | (curNo,i)<-zip[0..]initials]
  putStrLn $ "happiness: "++ show initHappiness
  descendant <- go 2 extra (zip[0..]initials)
  -- descendant is sorted desc by happiness
  return $ Right (map (\pos -> (pos, 1.0)) (head descendant)) -- FIXME
  where
    nMusician :: Int
    nMusician = length ms

    go :: Double -> Extra -> [(Int,[Point])] -> IO [[Point]]
    go count extra cands | count >= 16 = pure $ map snd cands
                         | otherwise  = do
      putStrLn $ "count: " ++ show count
      putStrLn "creating children..."
      children <- fmap (zip[truncate(count)*1000..]) $  genetic count cands
      happy <- sequence[happiness extra problem c curNo parNo| (curNo,(parNo,c))<-children]
      let hchildren = sortBy (flip compare`on`fst) (zip happy children)
      putStrLn $ "happiness: "++show (map fst hchildren)
      go (count+2) extra $ take 2 [(cno,c) |(h,(cno,(pno,c)))<-hchildren]

    genetic :: Double -> [(Int,[Point])] -> IO [(Int,[Point])]
    genetic count cands = do
      g <- newStdGen
      let dw = w/count
          dh = h/count
          rs = [((dw*(i-1)+zw, dw*i+zw),(dh*(j-1)+zh, dh*i+zh)) |[i,j]<-splitBy 2 (randomRs (0,count-1) g)]
      -- crossover c and d by area
      a1 <- sequence2 [(no, crossOver u v c d) | ([(no,c),(_,d)],(u,v))<-zip (sequence[cands,cands]) rs]
      -- replace n musicians randomly
      a2 <- sequence2 [(no, mutation problem n c) | let n=nMusician`div`(truncate count), (no,c)<-cands]
      return $ a1++a2

sequence2 :: [(a, IO (Maybe b))] -> IO [(a, b)]
sequence2 xs = do
  bs' <- sequence bs
  pure $ catMaybes $ zipWith (\a->fmap(a,)) as bs'
  where
    (as, bs) = unzip xs

splitBy :: Int -> [a] -> [[a]]
splitBy _ [] = []
splitBy n xs =
  let (as,bs) = splitAt n xs in as : splitBy n bs

toAnswer :: [Point] -> Answer
toAnswer ms = mkAnswer [Placement x y |(x,y)<-ms] (repeat 1)

happiness :: Extra -> Problem -> [Point] -> Int -> Int ->  IO Happiness
happiness extra problem ms curNo parNo =
  memoise extra' problem answer curNo parNo
  where
    extra' = updateExtra problem answer extra
    answer = toAnswer ms

-- randomly add musicians(targets) to the given placement(initial).
randomPlace :: Problem -> [(Int,Point)] -> [(Int,Point)] -> IO [Point]
randomPlace (Problem{stage_width=w, stage_height=h ,stage_bottom_left=(zw,zh)}) targets initial = do
  g <- newStdGen
  let randws = [fromIntegral(r+zw')| r<-randomRs (10,truncate(w-10)::Int) g, let zw'=truncate zw]
      randhs = [fromIntegral(r+zh')| r<-randomRs (10,truncate(h-10)::Int) g, let zh'=truncate zh]
  return $ map snd $ sortBy(compare`on`fst) $ randomPlace' targets maxRetry initial (zip randws randhs)
  where
    maxRetry :: Int
    maxRetry = 20

    randomPlace' :: [(Int,Point)] -> Int -> [(Int,Point)] -> [Point] -> [(Int,Point)]
    randomPlace' []               _ ps _   = ps
    randomPlace' ((k,current):ks) 0 ps kxy = randomPlace' ks maxRetry ((k,current):ps) kxy
    randomPlace' _                _ _  []  = error "can't happen"
    randomPlace' ((k,current):ks) r ps ((kx,ky):kxy) = do
--      traceShow k (return ())
      if (or [ isMusicianConflict (kx,ky) (x,y) | (_,(x,y))<-ps])
        then randomPlace' ((k,current):ks) (r-1) ps kxy --retry
        else randomPlace' ks maxRetry ((k,(kx,ky)):ps) kxy


isMusicianConflict :: Point -> Point -> Bool
isMusicianConflict (x1,y1) (x2,y2) =
  (x1-x2)^(2::Int) + (y1-y2)^(2::Int) < 100

-- embed bs' area(wl,wh,hl,hh) to as
crossOver :: (Double,Double) -> (Double,Double) -> [Point] -> [Point] -> IO (Maybe [Point])
crossOver (wl,wh) (hl,hh) as' bs' =
  return $ case crossOver' [] (zip[0..] $ zip as' bs') of
             [] -> Nothing
             rs -> Just rs
  where
    musicians :: [Int]
    musicians = [i |(i,(x,y))<-zip[0..]bs', wl<=x,x<=wh, hl<=y,y<=hh]

    crossOver' :: [Point] -> [(Int,(Point,Point))] -> [Point]
    crossOver' rs []             = reverse rs
    crossOver' rs ((i,(a,b)):xs) =
--      traceShow i $
      case (i`elem`musicians, or(map (isMusicianConflict b) rs)) of
        (False,_) -> crossOver' (a:rs) xs
        (_,False) -> crossOver' (b:rs) xs
        _         -> []

-- replace n musicians in as randomly
mutation :: Problem-> Int -> [Point] -> IO (Maybe [Point])
mutation problem@(Problem{musicians=ms}) n as = do
  g <- newStdGen
  let (rest,target) = partition (\(i,_)->i`elem`targets) (zip[0..]as)
      targets = nub $ take n $ randomRs (0,length ms-1) g
  fmap Just $ randomPlace problem target rest
