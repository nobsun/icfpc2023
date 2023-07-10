module Solver.Rectangle where

import Problem
import Solver (SolverF)

type Point = (Double, Double)

-- | ステージの外周を 10 間隔でミュージシャンを配置すると
--   内部のミュージシャンはすべてブロックされるので考慮する必要なし
--   外周のミュージシャンは隣のミュージシャンとの関係から
--   各ミュージシャンは左右 120 度の範囲にのみ影響する
--   角にいるミュージシャンは 210 度の範囲に影響する
--
-- 辺上にいるミュージシャンの影響を及ぼす範囲
--
--  西に配置されたミュージシャン (x0, y0) が影響を与える観客の条件:
--     - sqrt 3 (x - x0) - (y - y0) >= 0
--     - sqrt 3 (x - x0) + (y - y0) <= 0
--  北に配置されたミュージシャン (x0, y0) が影響を与える観客の条件:
--     - (x - x0) - sqrt 3 (y - y0) >= 0
--     - (x - x0) + sqrt 3 (y - y0) >= 0
--  東に配置されたミュージシャン (x0, y0) が影響を与える観客の条件:
--     - sqrt 3 (x - x0) - (y - y0) <= 0
--     - sqrt 3 (x - x0) + (y - y0) >= 0
--  南に配置されたミュージシャン (x0, y0) が影響を与える観客の条件:
--     - (x - x0) - sqrt 3 (y - y0) <= 0
--     - (x - x0) + sqrt 3 (y - y0) <= 0
--
-- 角にいるミュージシャンの影響を及ぼす範囲
--
-- 北西に配置されたミュージシャン (x0, y0) が影響を与える観客の条件:
--     - (x - x0) - sqrt 3 (y - y0) >= 0
--     - sqrt 3 (x - x0) - (y - y0) >= 0
-- 北東に配置されたミュージシャン (x0, y0) が影響を与える観客の条件:
--     - (x - x0) + sqrt 3 (y - y0) >= 0
--     - sqrt 3 (x - x0) + (y - y0) >= 0
-- 南東に配置されたミュージシャン (x0, y0) が影響を与える観客の条件:
--     - sqrt 3 (x - x0) - (y - y0) <= 0
--     - (x - x0) - sqrt 3 (y - y0) <= 0
-- 南西に配置されたミュージシャン (x0, y0) が影響を与える観客の条件:
--     - sqrt 3 (x - x0) + (y - y0) <= 0
--     - (x - x0) + sqrt 3 (y - y0) <= 0

inRangeOfWestMusician :: Point -> Point -> Bool
inRangeOfWestMusician (x0, y0) (x, y) =
  sqrt 3 * (x - x0) - (y - y0) >= 0 &&
  sqrt 3 * (x - x0) + (y - y0) <= 0

inRangeOfNorthMusician :: Point -> Point -> Bool
inRangeOfNorthMusician (x0, y0) (x, y) =
  (x - x0) - sqrt 3 * (y - y0) >= 0 &&
  (x - x0) + sqrt 3 * (y - y0) >= 0

inRangeOfEastMusician :: Point -> Point -> Bool
inRangeOfEastMusician (x0, y0) (x, y) =
  sqrt 3 * (x - x0) - (y - y0) <= 0 &&
  sqrt 3 * (x - x0) + (y - y0) >= 0

inRangeOfSouthMusician :: Point -> Point -> Bool
inRangeOfSouthMusician (x0, y0) (x, y) =
  (x - x0) - sqrt 3 * (y - y0) <= 0 &&
  (x - x0) + sqrt 3 * (y - y0) <= 0

inRangeOfNorthWestMusician :: Point -> Point -> Bool
inRangeOfNorthWestMusician (x0, y0) (x, y) =
  (x - x0) - sqrt 3 * (y - y0) >= 0 &&
  sqrt 3 * (x - x0) - (y - y0) >= 0

inRangeOfNorthEastMusician :: Point -> Point -> Bool
inRangeOfNorthEastMusician (x0, y0) (x, y) =
  (x - x0) + sqrt 3 * (y - y0) >= 0 &&
  sqrt 3 * (x - x0) + (y - y0) >= 0

inRangeOfSouthEastMusician :: Point -> Point -> Bool
inRangeOfSouthEastMusician (x0, y0) (x, y) =
  sqrt 3 * (x - x0) - (y - y0) <= 0 &&
  (x - x0) - sqrt 3 * (y - y0) <= 0

inRangeOfSouthWestMusician :: Point -> Point -> Bool
inRangeOfSouthWestMusician (x0, y0) (x, y) =
  sqrt 3 * (x - x0) + (y - y0) <= 0 &&
  (x - x0) + sqrt 3 * (y - y0) <= 0

stageBounds :: Problem -> (Double, Double, Double, Double)
stageBounds prob = (left, top, right, bottom)
  where
    (left, bottom) =  (stage_left prob, stage_bottom prob)
    (top, right) = (stage_top prob, stage_right prob)

data Direction = West
               | North
               | East
               | South
               | NorthWest
               | NorthEast
               | SouthEast
               | SouthWest
               | Inner
  deriving (Show, Eq)

positions :: Problem -> [(Point, Direction)]
positions prob = map withDirection poss
  where
    withDirection p@(x, y) = (p, direction p)
    direction (x, y)
      | x == westEdge && y == northEdge = NorthWest
      | x == eastEdge && y == northEdge = NorthEast
      | x == eastEdge && y == southEdge = SouthEast
      | x == westEdge && y == southEdge = SouthWest
      | x == westEdge = West
      | x == eastEdge = East
      | y == northEdge = North
      | y == southEdge = South
      | otherwise = Inner
    
    (w, n, e, s) = stageBounds prob
    poss = [ (x, y)
           | x <- [w+10.0, w+20.0 .. e-10.0]
           , y <- [n-10.0, n-20.0 .. s+10.0]
           , x <= e-10.0 && y >= s+10.0 -- Double なのでこれがないと誤差でステージに近すぎる場合が出る
           ]
    westEdge = minimum $ map fst poss
    eastEdge = maximum $ map fst poss
    northEdge = maximum $ map snd poss
    southEdge = minimum $ map snd poss
