module Solver.FrontHeat where

import Data.List (sortBy)

import Problem
import Solver (SolverF)

-- | フロントヒート戦略
-- Happiness の計算式が距離の2乗に反比例するのでステージに近い人のインパクトがより効きやすい
-- 遠くの人の場合には効果が薄くなる
-- ステージ上でも同様に前方(ステージ周囲など観客に近い場所)にミュージシャンはブロックされにくい。
-- そのため、ステージ周辺から観客の1割が存在するエリアをヒートエリアとして定義し、
-- ヒートエリアの人の好みの楽器を高い順に並べる
-- その楽器のミュージシャンを優先的に前方に配置する戦略をとる

-- | d がステージからの距離でどこまでヒートエリアとするかを指定している
frontAttendees :: Double -> Problem -> [Attendee]
frontAttendees d prob = filter inHeatArea (attendees prob)
  where
    inHeatArea (Attendee x y _) = w <= x && x <= e && s <= y && y <= n
    (w, n, e, s) = heatArea d prob

stageBounds :: Problem -> (Double, Double, Double, Double)
stageBounds prob = (left, top, right, bottom)
  where
    (left, bottom) =  (stage_left prob, stage_bottom prob)
    (top, right) = (stage_top prob, stage_right prob)

heatArea :: Double -> Problem -> (Double, Double, Double, Double)
heatArea d prob = (left, top, right, bottom)
  where
    (w, n, e, s) = stageBounds prob
    left = max (w - d) 0
    top = min (n + d) (room_height prob)
    right = min (e + d) (room_width prob)
    bottom = max (s - d) 0

decideFrontHeat :: Problem -> (Double, [Attendee])
decideFrontHeat prob = last xs
  where
    n = length (attendees prob)
    heatNum = ceiling (realToFrac n * 0.1) -- 1割がヒートエリア
    xs = takeWhile (\(_, xs) -> length xs <= heatNum) $ map (\d -> (d, frontAttendees d prob)) [1..]

-- | フロントヒートの好みの楽器を高いものから返す
--   数値は楽器のインデックス
tastesOfFrontHeat :: Problem -> [Int]
tastesOfFrontHeat prob = map fst orders
  where
    ave = map (\i -> average (map (!!i) ts)) [0..n-1]
    orders = sortBy (\x y -> compare (snd y) (snd x)) $ zip [0..] ave
    (_, attendees) = decideFrontHeat prob
    ts = map tastes attendees
    n = length $ head ts
    average xs = sum xs / realToFrac (length xs)
