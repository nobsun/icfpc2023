module Solver.Rectangle where

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
