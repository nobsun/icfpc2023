{-# LANGUAGE RecordWildCards #-}
module Happiness where

import Control.Concurrent (newChan, writeChan, readChan, forkIO, getNumCapabilities)
import Control.Monad
import Data.Array (Array)
import Data.Array.IArray ((!), listArray)
import Data.Array.Unboxed (UArray)
import Data.List.Split (chunksOf)

import Problem
import Answer
import Extra
import qualified BlockVec

-- | FIXME
type Happiness = Int

data HaStrategy
  = Naive
  | Parallel
  | WeightedAverage
  deriving (Eq, Show)

applyStrategy :: HaStrategy -> Extra -> Problem -> Answer -> IO Happiness
applyStrategy s e p a = (pure $!) =<< applyStrategyZ s e p a

applyStrategyZ :: HaStrategy -> Extra -> Problem -> Answer -> IO Happiness
applyStrategyZ Naive e p a = naive e p a
applyStrategyZ Parallel e p a = withQueue e p a
applyStrategyZ WeightedAverage e p a = weightedAverage e p a

squareDistance :: (Obstacle o1, Obstacle o2) => o1 -> o2 -> Double
squareDistance a b = (x1 - x2)^(2::Int) + (y1 - y2)^(2::Int)
  where
    (x1,y1) = obCenter a
    (x2,y2) = obCenter b

weightedAverageHappiness :: Problem -> Answer -> IO Happiness
weightedAverageHappiness prob ans = do
  extra <- mkExtra prob ans
  weightedAverage extra prob ans

weightedAverage :: Extra -> Problem -> Answer -> IO Happiness
weightedAverage extra prob ans = pure score
  where
    isBlock = isBlockWith (int_compat_blocktest extra) (answer_valid extra)
    score = sum [ impact 0 k
                | k <- [0..length ms-1], j <- [0..length ms-1]
                , not $ isBlock (ms !! k) (atnds !! 0) (ms !! j)
                ]
    atnds = [Attendee centerX centerY waTaste]
    ms = placements ans

    (centerX, centerY) = centerOfStage prob
    n = length atnds
    -- 観客の平均位置
    (attendeeX, attendeeY) = (x / conv n, y / conv n)
      where
        conv = fromRational . toRational
        (x, y) = foldr f (0.0, 0.0) atnds
          where f (Attendee x_ y_ _) (x', y') = (x_ + x', y_ + y')
    -- 観客の平均taste
    waTaste = foldr f [0.0..] (attendees prob)
      where
        f (Attendee x y ts) ts' = zipWith (\t t' -> w * t + t') ts ts'
          where
            d2 = (attendeeX - x)^(2::Int) + (attendeeY - y)^(2::Int)
            w = 1.0 / d2
    impact i k = ceiling $  num / den
      where
        num = 1e6 * (tastes (atnds !! i) !! (musicians prob !! k))
        den = squareDistance (ms !! k) (atnds !! i)

happiness :: Problem -> Answer -> IO Happiness
happiness prob ans = do
  extra <- mkExtra prob ans
  naive extra prob ans

-- | calculate happiness along with spec
naive :: Extra -> Problem -> Answer -> IO Happiness
naive extra prob ans = pure score
  where
    isBlock :: Obstacle o => Placement -> Attendee -> o -> Bool
    isBlock = isBlockWith (int_compat_blocktest extra) (answer_valid extra)
    score = sum [ impact (i, a_i) (k, inst_k, p_k)
                | (k, inst_k, p_k) <- zip3 [0 :: Int ..] (musicians prob) ms, (i, a_i) <- zip [0..] atnds
                , and [not $ isBlock p_k a_i p_j | (j, p_j) <- zip [0..] ms, k /= j]
                , and [not $ isBlock p_k a_i pl | pl <- plrs]
                ]
    atnds = attendees prob
    ms = placements ans
    ms_ar :: Array Int Placement
    ms_ar = listArray (0, length ms-1) ms
    insts = musicians prob
    plrs = pillars prob

    impact (i, a_i) (k, inst_k, p_k)
      | isFullDivisionProblem prob = ceiling $ closeness * fromIntegral (ceiling (num / den) :: Happiness)
      | otherwise = ceiling (num / den)
      where
        num = (million_times_atnds_tastes.problem_extra $ extra) ! i ! inst_k
        den = squareDistance p_k a_i
        closeness = 1 + 
          sum[ 1 / squareDistance p_k (ms_ar!j)
             | j <- (same_inst_musicians.problem_extra $ extra) ! inst_k
             , k /= j
             ]

withQueue :: Extra -> Problem -> Answer -> IO Happiness
withQueue extra prob ans = do
  nthread <- getNumCapabilities

  let inputs = zip3 [0 :: Int ..] (musicians prob) ms
      jobs = map chunk_score $ chunksOf chunkSize inputs
        where
          chunkSize = 1 `max` (length inputs `quot` chunks)
          chunks = nthread * 160
      size = length jobs

  inputQ <- newChan
  resultQ <- newChan

  let consumeJob = loop
        where loop = do
                thunk <- readChan inputQ
                thunk `seq` writeChan resultQ thunk
                loop

  () <$ replicateM nthread (forkIO consumeJob)

  mapM_  (writeChan inputQ) jobs  {- enqueue all jobs -}
  sum <$> replicateM size (readChan resultQ)  {- dequeue all results and sum of them -}
  where
    isBlock :: Obstacle o => Placement -> Attendee -> o -> Bool
    isBlock = isBlockWith (int_compat_blocktest extra) (answer_valid extra)
    chunk_score triple_chunk =
      sum
      [ impact (i, a_i) (k, inst_k, p_k)
      | (k, inst_k, p_k) <- triple_chunk
      , (i, a_i) <- zip [0..] atnds
      , and [not $ isBlock p_k a_i p_j | (j, p_j) <- zip [0..] ms, k /= j]
      , and [not $ isBlock p_k a_i pl | pl <- plrs]
      ]

    atnds = attendees prob
    ms = placements ans
    ms_ar :: Array Int Placement
    ms_ar = listArray (0, length ms-1) ms
    insts = musicians prob
    plrs = pillars prob

    impact (i, a_i) (_k, inst_k, p_k) = ceiling $ (closeness * num) / den
      where
        num = (million_times_atnds_tastes.problem_extra $ extra)! i ! inst_k
        den = squareDistance p_k a_i
        closeness = 1 + 
          sum[ 1/(squareDistance p_k (ms_ar!j))
             | inst<-insts, inst/=inst_k
             , j <-(same_inst_musicians.problem_extra $ extra)! inst
             ]

isBlockWith :: Obstacle o => BlockTestICompat -> AnswerCheck -> Placement -> Attendee -> o -> Bool
isBlockWith IntCompat    Valid    = isBlockInt
isBlockWith NotIntCompat Valid    = isBlockDouble
isBlockWith IntCompat    Invalid  = isBlockIntInvalid
isBlockWith NotIntCompat Invalid  = isBlockDoubleInvalid

-- | isBlockInt
--   musician と attendee の直線に blocker が距離 5 以内にいる
--   かつ
--   blocker から直線におろした垂線の交点が musician と attendee の間にあること
--   を判定する
--     musician (mx, my)
--     attendee (ax, ay)
--     blocker  (bx, by)
--
-- 線分の端点が一致した場合は垂線が通る
-- >>> isBlockIntInvalid (Placement 0.0 0.0) (Attendee 1.0 1.0 []) (Placement 0.0 0.0)
-- True
-- >>> isBlockIntInvalid (Placement 0.0 0.0) (Attendee 1.0 1.0 []) (Placement 1.0 1.0)
-- True
--
-- 距離が 5 より離れている
-- >>> isBlockIntInvalid (Placement 0.0 0.0) (Attendee 1.0 1.0 []) (Placement 7.0 8.0)
-- False
-- >>> isBlockIntInvalid (Placement 0.0 0.0) (Attendee 1.0 1.0 []) (Placement (-8.0) (-7.0))
-- False
-- >>> isBlockIntInvalid (Placement 0.0 0.0) (Attendee 1.0 1.0 []) (Placement (0.0) (6.0))
-- False
-- >>> isBlockIntInvalid (Placement 0.0 0.0) (Attendee 1.0 1.0 []) (Placement (0.0) (7.0))
-- False
--
-- (4.0, 6.0) までそれぞれ、 4, 5, 6
-- >>> isBlockDouble (Placement 0.0 6.0) (Attendee 8.0 6.0 []) (Placement 4.0 2.0)
-- True
-- >>> isBlockDouble (Placement 0.0 6.0) (Attendee 8.0 6.0 []) (Placement 4.0 1.0)
-- True
-- >>> isBlockDouble (Placement 0.0 6.0) (Attendee 8.0 6.0 []) (Placement 4.0 0.0)
-- False
--
-- 斜辺の真ん中までそれぞれ、 3√2, 5, 5, 4√2
-- >>> isBlockDouble (Placement 1.0 7.0) (Attendee 7.0 1.0 []) (Placement 1.0 1.0)
-- True
-- >>> isBlockDouble (Placement 1.0 7.0) (Attendee 7.0 1.0 []) (Placement 1.0 0.0)
-- True
-- >>> isBlockDouble (Placement 1.0 7.0) (Attendee 7.0 1.0 []) (Placement 0.0 1.0)
-- True
-- >>> isBlockDouble (Placement 1.0 7.0) (Attendee 7.0 1.0 []) (Placement 0.0 0.0)
-- False
--
isBlockInt :: Obstacle o => Placement -> Attendee -> o -> Bool
isBlockInt (Placement mx my) (Attendee ax ay _) obs =
  isBlockInt' (floor mx, floor my) (floor ax, floor ay) (floor bx, floor by) (floor br)
  where
    (bx,by) = obCenter obs
    br = obRadius obs

isBlockInt' :: (Int, Int) -> (Int, Int) -> (Int, Int) -> Int -> Bool
isBlockInt' = BlockVec.isBlock

-- | isBlockDouble
--   isBlockInt の Int には特化していないバージョン
--
-- 線分の端点が一致した場合は垂線が通る
-- >>> isBlockIntInvalid (Placement 0.0 0.0) (Attendee 1.0 1.0 []) (Placement 0.0 0.0)
-- True
-- >>> isBlockIntInvalid (Placement 0.0 0.0) (Attendee 1.0 1.0 []) (Placement 1.0 1.0)
-- True
--
-- 距離が 5 より離れている
-- >>> isBlockIntInvalid (Placement 0.0 0.0) (Attendee 1.0 1.0 []) (Placement 7.0 8.0)
-- False
-- >>> isBlockIntInvalid (Placement 0.0 0.0) (Attendee 1.0 1.0 []) (Placement (-8.0) (-7.0))
-- False
-- >>> isBlockIntInvalid (Placement 0.0 0.0) (Attendee 1.0 1.0 []) (Placement (0.0) (6.0))
-- False
-- >>> isBlockIntInvalid (Placement 0.0 0.0) (Attendee 1.0 1.0 []) (Placement (0.0) (7.0))
-- False
--
-- (4.0, 6.0) までそれぞれ、 4, 5, 6
-- >>> isBlockDouble (Placement 0.0 6.0) (Attendee 8.0 6.0 []) (Placement 4.0 2.0)
-- True
-- >>> isBlockDouble (Placement 0.0 6.0) (Attendee 8.0 6.0 []) (Placement 4.0 1.0)
-- True
-- >>> isBlockDouble (Placement 0.0 6.0) (Attendee 8.0 6.0 []) (Placement 4.0 0.0)
-- False
--
-- 斜辺の真ん中までそれぞれ、 3√2, 5, 5, 4√2
-- >>> isBlockDouble (Placement 1.0 7.0) (Attendee 7.0 1.0 []) (Placement 1.0 1.0)
-- True
-- >>> isBlockDouble (Placement 1.0 7.0) (Attendee 7.0 1.0 []) (Placement 1.0 0.0)
-- True
-- >>> isBlockDouble (Placement 1.0 7.0) (Attendee 7.0 1.0 []) (Placement 0.0 1.0)
-- True
-- >>> isBlockDouble (Placement 1.0 7.0) (Attendee 7.0 1.0 []) (Placement 0.0 0.0)
-- False
--
isBlockDouble :: Obstacle o => Placement -> Attendee -> o -> Bool
isBlockDouble (Placement mx my) (Attendee ax ay _) obs =
  BlockVec.isBlock (mx, my) (ax, ay) (bx, by) br
  where
    (bx,by) = obCenter obs
    br = obRadius obs

-- | isBlockIntInvalid
--   isBlockInt の valid でない answer に対応したバージョン
--
-- >>> isBlockIntInvalid (Placement 0.0 0.0) (Attendee 1.0 1.0 []) (Placement 2.0 2.0)
-- True
-- >>> isBlockIntInvalid (Placement 0.0 0.0) (Attendee 1.0 1.0 []) (Placement 1.0 2.0)
-- True
-- >>> isBlockIntInvalid (Placement (-1.0) 1.0) (Attendee 0.0 3.0 []) (Placement 0.0 0.0)
-- True
-- >>> isBlockIntInvalid (Placement 0.0 0.0) (Attendee 1.0 1.0 []) (Placement (0.0) (5.0))
-- True
--
isBlockIntInvalid :: Obstacle o => Placement -> Attendee -> o -> Bool
isBlockIntInvalid (Placement mx my) (Attendee ax ay _) obs =
  isBlockIntInvalid' (floor mx, floor my) (floor ax, floor ay) (floor bx, floor by) (floor br)
  where
    (bx,by) = obCenter obs
    br = obRadius obs

isBlockIntInvalid' :: (Int, Int) -> (Int, Int) -> (Int, Int) -> Int -> Bool
isBlockIntInvalid' = BlockVec.isBlockWithoutValid

-- | isBlockIntInvalid
--   isBlockDouble の valid でない answer に対応したバージョン
--
-- >>> isBlockIntInvalid (Placement 0.0 0.0) (Attendee 1.0 1.0 []) (Placement 2.0 2.0)
-- True
-- >>> isBlockIntInvalid (Placement 0.0 0.0) (Attendee 1.0 1.0 []) (Placement 1.0 2.0)
-- True
-- >>> isBlockIntInvalid (Placement (-1.0) 1.0) (Attendee 0.0 3.0 []) (Placement 0.0 0.0)
-- True
-- >>> isBlockIntInvalid (Placement 0.0 0.0) (Attendee 1.0 1.0 []) (Placement (0.0) (5.0))
-- True
--
isBlockDoubleInvalid :: Obstacle o => Placement -> Attendee -> o -> Bool
isBlockDoubleInvalid (Placement mx my) (Attendee ax ay _) obs =
  BlockVec.isBlockWithoutValid (mx, my) (ax, ay) (bx, by) br
  where
    (bx,by) = obCenter obs
    br = obRadius obs

-- |
--
-- >>> isBlock' (0.0, 0.0) (1.0, 1.0) (2.0, 2.0)
-- True
-- >>> isBlock' (0.0, 0.0) (1.0, 1.0) (1.0, 1.0)
-- True
-- >>> isBlock' (0.0, 0.0) (1.0, 1.0) (1.0, 2.0)
-- True
-- >>> isBlock' (-1.0, 1.0) (0.0, 3.0) (0.0, 0.0)
-- True
-- >>> isBlock' (0.0, 0.0) (1.0, 1.0) (7.0, 8.0)
-- False
-- >>> isBlock' (0.0, 0.0) (1.0, 1.0) (-8.0, -7.0)
-- False
-- >>> isBlock' (0.0, 0.0) (1.0, 1.0) ((0.0), (5.0))
-- True
-- >>> isBlock' (0.0, 0.0) (1.0, 1.0) ((0.0), (8.0))
-- False
--
{-
-- NOT PASS -- >>> isBlock' (0.0, 0.0) (1.0, 1.0) ((0.0), (6.0))
-- NOT PASS -- False
-- NOT PASS -- >>> isBlock' (0.0, 0.0) (1.0, 1.0) ((0.0), (7.0))
-- NOT PASS -- False
 -}
isBlock' :: (RealFrac a, Floating a) => (a, a) -> (a, a) -> (a, a) -> Bool
isBlock' = isBlockWithRadius' 5.0

isBlockWithRadius' :: (RealFrac a, Floating a) => a -> (a, a) -> (a, a) -> (a, a) -> Bool
isBlockWithRadius' radius (mx, my) (ax, ay) (bx, by)
  = distance (a, b, c) (bx, by) <= radius &&
    (
      between (mx, my) (p, q) (ax, ay) ||
      -- この条件は交点が直線の間にないけど線分にかかるので必要なように思う
      inner (p, q) (mx, my) || inner (p, q) (ax, ay)
    )
  where
    -- mx, my と ax, ay を通る直線の方程式
    -- a * x + b * y + c = 0
    -- この a, b, c を求める
    (a, b, c) = line (mx, my) (ax, ay)
    -- (x1, y1) (x2, y2) を通る直線へ (x0, y0) からおろした垂線の交点を (p, q) とする
    (p, q)
      | a == 0 = (bx, - c / b)
      | b == 0 = (- c / a, by)
      | otherwise = (p_, q_)
      where
        -- 垂線の直線は y = (b/a) x + d になる
        d = by - (b / a) * bx
        -- a p + b q + c = 0 と
        -- q = (b/a) p + d
        -- の連立方程式を解く
        p_ = a / (a^(2::Int) + b^(2::Int)) * (- c - b * d)
        q_ = (b / a) * p + d

    -- (x2, y2) が (x1, y1) の radius 以内にあるかどうか
    inner (x1, y1) (x2, y2) = sqrt ((x2 - x1)^(2::Int) + (y2 - y1)^(2::Int)) <= radius


-- | (x1, y1) (x2, y2) を通る直線の方程式の係数 a, b, c を求める
--   傾き a = (y2 - y1) / (x2 - x1) として
--   y = a * (x - x1) + y1
--
-- >>> line (0.0, 0.0) (1.0, 1.0)
-- (1.0,-1.0,0.0)
-- >>> line (0.0, 0.0) (1.0, 2.0)
-- (2.0,-1.0,0.0)
-- >>> line (0.0, 0.0) (2.0, 1.0)
-- (1.0,-2.0,0.0)
-- >>> line (-1.0, 1.0) (0.0, 3.0)
-- (2.0,-1.0,3.0)
--
line :: Num a => (a, a) -> (a, a) -> (a, a, a)
line (x1, y1) (x2, y2) = (y2 - y1, x1 - x2, x2 * y1 - x1 * y2)

-- | また直線 a * x + b * y + c = 0 と 点 (x0, y0) の距離 d
-- >>> distance (1.0, -1.0, 0.0) (0.0, 0.0)
-- 0.0
-- >>> distance (1.0, -1.0, 0.0) (1.0, 0.0)
-- 0.7071067811865475
-- >>> distance (1.0, -1.0, 0.0) (0.0, 1.0)
-- 0.7071067811865475
-- >>> distance (1.0, -1.0, 0.0) (1.0, 1.0)
-- 0.0
distance :: (Fractional a, Floating a) => (a, a, a) -> (a, a) -> a
distance (a, b, c) (x0, y0) = abs (a * x0 + b * y0 + c) / sqrt (a * a + b * b)

-- | (x0, y0) が (x1, y1) (x2, y2) の間にあるかどうか
-- >>> between (0.0, 0.0) (1.0, 1.0) (2.0, 2.0)
-- True
-- >>> between (0.0, 0.0) (1.0, 1.0) (1.0, 1.0)
-- True
-- >>> between (0.0, 0.0) (1.0, 1.0) (1.0, 2.0)
-- True
-- >>> between (0.0, 0.0) (0.0, 0.0) (1.0, 2.0)
-- True
-- >>> between (0.0, 0.0) (-1.0, 0.0) (2.0, 1.0)
-- False
-- >>> between (0.0, 0.0) (0.0, -2.0) (2.0, 1.0)
-- False
-- >>> between (0.0, 0.0) (-1.0, 2.0) (2.0, 1.0)
-- False
-- >>> between (0.0, 0.0) (1.0, -0.5) (2.0, 1.0)
-- False
between :: Real a => (a, a) -> (a, a) -> (a, a) -> Bool
between (x1, y1) (x0, y0) (x2, y2)
  = (x0 - x1) * (x0 - x2) <= 0 && (y0 - y1) * (y0 - y2) <= 0
