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

-- musician と attendee の直線に blocker が 5 以内にいる
-- かつ blocker から直線におろした垂線の交点が musician と attendee の間にあること
-- を判定する
--   musician (mx, my)
--   attendee (ax, ay)
--   blocker  (bx, by)
isBlock :: Placement -> Attendee -> Placement -> Bool
isBlock (Placement mx my) (Attendee ax ay _) (Placement bx by)
  = distance (a, b, c) (bx, by) <= 5.0 && between (mx, my) (p, q) (ax, ay)
  where
    -- mx, my と ax, ay を通る直線の方程式
    -- a * x + b * y + c = 0
    -- この a, b, c を求める
    (a, b, c) = line (mx, my) (ax, ay)
    -- (x1, y1) (x2, y2) を通る直線へ (x0, y0) からおろした垂線の交点を (p, q) とする
    (p, q) = (a * k + bx, b * k + by)
      where
        k = (a * bx + b * by + c) / (a * a + b * b)
    
    -- (x1, y1) (x2, y2) を通る直線の方程式の係数 a, b, c を求める
    -- 傾き a = (y2 - y1) / (x2 - x1) として
    -- y = a * (x - x1) + y1
    line (x1, y1) (x2, y2) = (y2 - y1, x1 - x2, x2 * y1 - x1 * y2)
    -- また直線 a * x + b * y + c = 0 と 点 (x0, y0) の距離 d
    distance (a, b, c) (x0, y0) = abs (a * x0 + b * y0 + c) / sqrt (a * a + b * b)

    -- (x0, y0) が (x1, y1) (x2, y2) の間にあるかどうか
    between (x1, y1) (x0, y0) (x2, y2)
      = (x0 - x1) * (x0 - x2) <= 0 && (y0 - y1) * (y0 - y2) <= 0
