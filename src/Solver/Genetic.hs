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
import Extra
import Happiness (Happiness, weightedAverage)
import Solver (SolverF)


type Point = (Double,Double)

infixr 0 $$
($$) :: Show a => (a -> b) -> a -> b
f $$ x = traceShow x (f x)


getCandidates :: SolverF
getCandidates problem = unsafePerformIO (getCandidatesIO problem)

getCandidatesIO :: Problem -> IO (Either String [Point])
getCandidatesIO problem@(Problem{stage_width=w, stage_height=h ,stage_bottom_left=(zw,zh), musicians=ms}) = do
  putStrLn $ "stage-width: "++show w++", stage_height: "++show h++", musicians: "++show nMusician
  putStrLn $ "generating initial placement randomly"
  initials <- sequence $ take 2 $ repeat $ randomPlace problem (zip[0..nMusician-1](repeat undefined)) []
  extra <- mkExtra problem (toAnswer (initials!!0))
  initHappiness <- sequence[happiness extra problem i | i<-initials]
  putStrLn $ "happiness: "++ show initHappiness
  descendant <- go 2 extra initials
  -- descendant is sorted desc by happiness
  return $ Right (head descendant)
  where
    nMusician :: Int
    nMusician = length ms

    go :: Double -> Extra -> [[Point]] -> IO [[Point]]
    go count extra cands | count >= 16 = return cands
                         | otherwise  = do
      putStrLn $ "count: " ++ show count
      putStrLn "creating children..."
      children <- genetic count cands
      happy <- sequence[happiness extra problem c | c<-children]
      let hchildren = sortBy (flip compare`on`fst) (zip happy children)
      putStrLn $ "happiness: "++show (map fst hchildren)
      go (count+2) extra (map snd $ take 2 hchildren)

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
splitBy _ [] = []
splitBy n xs =
  let (as,bs) = splitAt n xs in as : splitBy n bs

toAnswer :: [Point] -> Answer
toAnswer ms = Answer{placements=[Placement x y |(x,y)<-ms]}

happiness :: Extra -> Problem -> [Point] -> IO Happiness
happiness extra problem ms =
  weightedAverage extra' problem answer
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
