module Solver.FrontHeat where

import Data.List (sortBy)
import qualified Data.Map as Map

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


-- | ステージ上の立てる場所とその場所の楽器の優先度リストとを返す
standingPositions :: (Double, [Attendee]) -> Problem -> [((Double, Double), [Instrument])]
standingPositions (d, atnds) prob = map (\pos -> (pos, preferedInstrs pos (near pos atnds))) poss
  where
    (w, n, e, s) = stageBounds prob
    poss = [ (x, y)
           | x <- [w+10.0, w+20.0 .. e-10.0]
           , y <- [s+10.0, s+20.0 .. n-10.0]
           , x <= e-10.0 && y <= n-10.0 -- Double なのでこれがないと誤差でステージに近すぎる場合が出る
           ]
    near (x, y) = filter (\(Attendee x' y' _) -> (x-x')^2 + (y-y')^2 <= (d+10)^2)
    preferedInstrs (x, y) = map fst . sortBy descByLike . zip [0..] . summary
      where descByLike x y = compare (snd y) (snd x)
            summary = foldr (\(Attendee x' y' ts) acc -> zipWith (calc x' y') acc ts) (repeat 0.0)
              where
                calc :: Double -> Double -> Double -> Double -> Double
                calc x' y' acc t = acc + t / ((x-x')^2 + (y-y')^2)

heatArea :: Double -> Problem -> (Double, Double, Double, Double)
heatArea d prob = (left, top, right, bottom)
  where
    (w, n, e, s) = stageBounds prob
    left = max (w - d) 0
    top = min (n + d) (room_height prob)
    right = min (e + d) (room_width prob)
    bottom = max (s - d) 0

-- | rate は観客の何割を意識するか
-- rate [ 0.1 .. 0.9 ] で試してみると良い
decideFrontHeat :: Double -> Problem -> (Double, [Attendee])
decideFrontHeat rate prob = last xs
  where
    n = length (attendees prob)
    heatNum = ceiling (realToFrac n * rate) -- 1割がヒートエリア
    xs = takeWhile (\(_, xs) -> length xs <= heatNum) $ map (\d -> (d, frontAttendees d prob)) [1..]

-- | フロントヒートの好みの楽器を高いものから返す
--   数値は楽器のインデックス
tastesOfFrontHeat :: [Attendee] -> Problem -> [Instrument]
tastesOfFrontHeat atnds prob = map fst orders
  where
    ave = map (\i -> average (map (!!i) ts)) [0..n-1]
    orders = sortBy (\x y -> compare (snd y) (snd x)) $ zip [0..] ave
    ts = map tastes atnds
    n = length $ head ts
    average xs = sum xs / realToFrac (length xs)

-- | フロントヒートの好みの楽器を演奏するミュージシャン順に分離しつつ並べる
popularMusicians :: Problem -> [Instrument] -> [(Instrument, [Int])]
popularMusicians prob instrs = splitWithOrder instrs ms
  where
    ms = zip [0..] (musicians prob)

splitWithOrder :: [Instrument] -> [(Int, Instrument)] -> [(Instrument, [Int])]
splitWithOrder instrs ms = map (\instr -> (instr, m Map.! instr)) instrs
  where
    seed = Map.fromList $ map (,[]) instrs
    m = foldl (\m' (i, instr) -> Map.insertWith (++) instr [i] m') seed ms

matching :: [(Instrument, [Int])] -> [((Double, Double), [Instrument])] -> [(Int, (Double, Double))]
matching ms poss = zipWith (\i (pos, _) -> (i, pos)) (concatMap snd ms) poss


getCandidates :: SolverF
getCandidates prob = Right $ map snd res
  where
     (d, atnds) = decideFrontHeat 0.3 prob
     tofh = tastesOfFrontHeat atnds prob
     poss = standingPositions (d, atnds) prob
     ms = popularMusicians prob tofh
     assignment = matching ms poss
     res = sortBy (\x y -> compare (fst x) (fst y)) assignment
