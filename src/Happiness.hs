{-# LANGUAGE RecordWildCards #-}
module Happiness where

import Data.Array (Array)
import Data.Array.IArray ((!), listArray)
import Data.Array.Unboxed (UArray)
import Data.List (delete, zip3)

import Problem
import Answer
import qualified BlockVec
import Control.Concurrent (setNumCapabilities)

-- | FIXME
type Happiness = Integer

data HaStrategy
  = Naive
  | WeightedAverage
  deriving (Eq, Show)

applyHappiness :: HaStrategy -> Problem -> Answer -> Happiness
applyHappiness Naive = happiness
applyHappiness WeightedAverage = weightedAverageHappiness

squareDistance :: Placement -> Attendee -> Double
squareDistance (Placement x1 y1) (Attendee x2 y2 _) = (x1 - x2)^(2::Int) + (y1 - y2)^(2::Int)

weightedAverageHappiness :: Problem -> Answer -> Happiness
weightedAverageHappiness prob ans = score
  where
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
          where f (Attendee x y _) (x', y') = (x + x', y + y')
    -- 観客の平均taste
    waTaste = foldr f [0.0..] (attendees prob)
      where
        f (Attendee x y ts) ts' = zipWith (\t t' -> w * t + t') ts ts'
          where
            d2 = (attendeeX - x)^2 + (attendeeY - y)^2
            w = 1.0 / d2
    impact i k = ceiling $  num / den
      where
        num = 1e6 * (tastes (atnds !! i) !! (musicians prob !! k))
        den = squareDistance (ms !! k) (atnds !! i)

-- | calculate happiness
happiness :: Problem -> Answer -> Happiness
happiness prob ans = score
  where
    score = sum [ impact (i, a_i) (k, inst_k, p_k)
                | (k, inst_k, p_k) <- zip3 [0..] (musicians prob) ms, (i, a_i) <- zip [0..] atnds
                , and [not $ isBlock p_k a_i p_j | (j, p_j) <- zip [0..] ms, k /= j]
                ]
    atnds = attendees prob
    ms = placements ans

    {- each tastes times 1,000,000 memos -}
    million_times_tastes :: Attendee -> UArray Int Double
    million_times_tastes a = listArray (0, length ts - 1) $ map (1e6 *) ts  where ts = tastes a

    million_times_atnds_tastes :: Array Int (UArray Int Double)
    million_times_atnds_tastes = listArray (0, length atnds - 1) $ map million_times_tastes atnds

    impact (i, a_i) (k, inst_k, p_k) = ceiling $ num / den
      where
        num = million_times_atnds_tastes ! i ! inst_k
        den = squareDistance p_k a_i

-- | musician と attendee の直線に blocker が 5 以内にいる
--   かつ blocker から直線におろした垂線の交点が musician と attendee の間にあること
--   を判定する
--     musician (mx, my)
--     attendee (ax, ay)
--     blocker  (bx, by)
--
-- >>> isBlock (Placement 0.0 0.0) (Attendee 1.0 1.0 []) (Placement 2.0 2.0)
-- True
-- >>> isBlock (Placement 0.0 0.0) (Attendee 1.0 1.0 []) (Placement 1.0 1.0)
-- True
-- >>> isBlock (Placement 0.0 0.0) (Attendee 1.0 1.0 []) (Placement 1.0 2.0)
-- True
-- >>> isBlock (Placement (-1.0) 1.0) (Attendee 0.0 3.0 []) (Placement 0.0 0.0)
-- True
-- >>> isBlock (Placement 0.0 0.0) (Attendee 1.0 1.0 []) (Placement 7.0 8.0)
-- False
-- >>> isBlock (Placement 0.0 0.0) (Attendee 1.0 1.0 []) (Placement (-8.0) (-7.0))
-- False
-- >>> isBlock (Placement 0.0 0.0) (Attendee 1.0 1.0 []) (Placement (0.0) (5.0))
-- True
-- >>> isBlock (Placement 0.0 0.0) (Attendee 1.0 1.0 []) (Placement (0.0) (6.0))
-- False
-- >>> isBlock (Placement 0.0 0.0) (Attendee 1.0 1.0 []) (Placement (0.0) (7.0))
-- False
--
isBlock :: Placement -> Attendee -> Placement -> Bool
isBlock (Placement mx my) (Attendee ax ay _) (Placement bx by) = BlockVec.isBlock' (mx, my) (ax, ay) (bx, by)

isBlockViaInt :: (Double, Double) -> (Double, Double) -> (Double, Double) -> Bool
isBlockViaInt (mx, my) (ax, ay) (bx, by) = isBlockInt (floor mx, floor my) (floor ax, floor ay) (floor bx, floor by)

isBlockInt :: (Int, Int) -> (Int, Int) -> (Int, Int) -> Bool
isBlockInt = BlockVec.isBlock'

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
isBlock' (mx, my) (ax, ay) (bx, by)
  = distance (a, b, c) (bx, by) <= 5.0 &&
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
      | otherwise = (p, q)
      where
        -- 垂線の直線は y = (b/a) x + d になる
        d = by - (b / a) * bx
        -- a p + b q + c = 0 と
        -- q = (b/a) p + d
        -- の連立方程式を解く
        p = a / (a^(2::Int) + b^(2::Int)) * (- c - b * d)
        q = (b / a) * p + d

    -- (x2, y2) が (x1, y1) の 5.0 以内にあるかどうか
    inner (x1, y1) (x2, y2) = sqrt ((x2 - x1)^(2::Int) + (y2 - y1)^(2::Int)) <= 5.0

-- >>> isBlock2 (Placement 0.0 0.0) (Attendee 1.0 1.0 []) (Placement 2.0 2.0)
-- True
-- >>> isBlock2 (Placement 0.0 0.0) (Attendee 1.0 1.0 []) (Placement 1.0 1.0)
-- True
-- >>> isBlock2 (Placement 0.0 0.0) (Attendee 1.0 1.0 []) (Placement 1.0 2.0)
-- True
-- >>> isBlock2 (Placement (-1.0) 1.0) (Attendee 0.0 3.0 []) (Placement 0.0 0.0)
-- True
-- >>> isBlock2 (Placement 0.0 0.0) (Attendee 1.0 1.0 []) (Placement 7.0 8.0)
-- False
-- >>> isBlock2 (Placement 0.0 0.0) (Attendee 1.0 1.0 []) (Placement (-8.0) (-7.0))
-- False
-- >>> isBlock2 (Placement 0.0 0.0) (Attendee 1.0 1.0 []) (Placement (0.0) (5.0))
-- True
-- >>> isBlock2 (Placement 0.0 0.0) (Attendee 1.0 1.0 []) (Placement (0.0) (6.0))
-- False
-- >>> isBlock2 (Placement 0.0 0.0) (Attendee 1.0 1.0 []) (Placement (0.0) (7.0))
-- False
isBlock2 :: Placement -> Attendee -> Placement -> Bool
isBlock2 (Placement mx my) (Attendee ax ay _) (Placement bx by) =
  inTriangle ml mr al (bx,by) || inTriangle al ar mr (bx,by)
  where
    r = -(mx-ax)/(ay-ay)
    dx = 5/sqrt(1+r^2)
    dy = r*dx

    (ml,mr) = ((mx+dx,my+dy) , (mx-dx,my-dy))
    (al,ar) = ((ax+dx,ay+dy) , (ax-dx,ay-dy))

-- p is in traiangle (a,b,c)?
inTriangle a b c p =
  and(map(>0)zs) || and(map(<0)zs)
  where
    zs = map (crossz p) [a,b,c]

-- z of cross product
crossz (a1,a2) (b1,b2) =
  a1*b2 - a2*b1

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
