module Happiness where

import Data.List (delete)

import Problem
import Answer

-- | FIXME
type Happiness = Integer

-- | calculate happiness
happiness :: Problem -> Answer -> Happiness
happiness prob ans = score
  where
    score = sum [ impact i k
                | k <- [0..length ms-1], i <- [0..length atnds-1], j <- [0..length ms-1]
                , i /= j
                , not $ isBlock (ms !! k) (atnds !! i) (ms !! j)
                ]
    atnds = attendees prob
    ms = placements ans
    squareDistance (Placement x1 y1) (Attendee x2 y2 _) = (x1 - x2)^2 + (y1 - y2)^2
    impact i k = ceiling $  num / den
      where
        num = 10^6 * (tastes (atnds !! i) !! (musicians prob !! k))
        den = squareDistance (ms !! k) (atnds !! i)

-- musician と attendee の直線から 5 以下の距離に blocker がいるかどうか
--   musician (mx, my)
--   attendee (ax, ay)
--   blocker  (bx, by)
isBlock :: Placement -> Attendee -> Placement -> Bool
isBlock (Placement mx my) (Attendee ax ay _) (Placement bx by)
  = distance (line (mx, my) (ax, ay)) (bx, by) <= 5.0
  where
    -- (x1, y1) (x2, y2) を通る直線の方程式
    -- 傾き a = (y2 - y1) / (x2 - x1) として
    -- y = a * (x - x1) + y1
    line (x1, y1) (x2, y2) = (a, b, c)
      where
        a = y2 - y1
        b = x1 - x2
        c = x2 * y1 - x1 * y2
    -- また直線 a * x + b * y + c = 0 と 点 (x0, y0) の距離 d
    distance (a, b, c) (x0, y0) = abs (a * x0 + b * y0 + c) / sqrt (a * a + b * b)

