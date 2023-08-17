module Solver.FrontHeat where

import Data.List (sortBy, foldl')
import qualified Data.Map as Map
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Unboxed as VU

import Problem
import Solver (SolverF)

-- | フロントヒート戦略
-- Happiness の計算式が距離の2乗に反比例するのでステージに近い人のインパクトがより効きやすい
-- 遠くの人の場合には効果が薄くなる
-- ステージ上でも同様に前方(ステージ周囲など観客に近い場所)にミュージシャンはブロックされにくい。
-- そのため、ステージ周辺から観客の1割が存在するエリアをヒートエリアとして定義し、
-- ヒートエリアの人の好みの楽器を高い順に並べる
-- その楽器のミュージシャンを優先的に前方に配置する戦略をとる

-- | rate は観客の何割を意識するか
-- rate [ 0.1 .. 0.9 ] で試してみると良い
decideFrontHeat :: Double -> Problem -> (Double, [Attendee])
decideFrontHeat rate prob = last xs
  where
    n = length (attendees prob)
    heatNum = ceiling (realToFrac n * rate) -- 1割がヒートエリア
    xs = takeWhile (\(_, xs) -> length xs <= heatNum) $ map (\d -> (d, frontAttendees d prob)) [1..]

-- | d がステージからの距離でどこまでヒートエリアとするかを指定している
frontAttendees :: Double -> Problem -> [Attendee]
frontAttendees d prob = filter inHeatArea $ VG.toList $ attendees prob
  where
    inHeatArea (Attendee x y _) = w <= x && x <= e && s <= y && y <= n
    (w, n, e, s) = heatArea d prob

heatArea :: Double -> Problem -> (Double, Double, Double, Double)
heatArea d prob = (left, top, right, bottom)
  where
    (w, n, e, s) = stageBounds prob
    left = max (w - d) 0
    top = min (n + d) (room_height prob)
    right = min (e + d) (room_width prob)
    bottom = max (s - d) 0

stageBounds :: Problem -> (Double, Double, Double, Double)
stageBounds prob = (left, top, right, bottom)
  where
    (left, bottom) =  (stage_left prob, stage_bottom prob)
    (top, right) = (stage_top prob, stage_right prob)


-- | ステージ上の立てる場所を返す
--   それぞれの場所について、その場所にいる人たちの好みの楽器を高い順に並べる
--   Like がインパクトなので、Like が高い楽器を優先的に配置したい
standingPositions :: (Double, [Attendee]) -> Problem -> [((Double, Double), [(Instrument, Like)])]
standingPositions (d, atnds) prob
  = sortBy (\(_, (_, l1):_) (_, (_, l2):_) -> compare l2 l1)
    $ map (\pos -> (pos, preferedInstrs pos (near pos atnds))) poss
  where
    (w, n, e, s) = stageBounds prob
    poss = [ (x, y)
           | x <- [w+10.0, w+20.0 .. e-10.0]
           , y <- [s+10.0, s+20.0 .. n-10.0]
           , x <= e-10.0 && y <= n-10.0 -- Double なのでこれがないと誤差でステージに近すぎる場合が出る
           ]
    near (x, y) = filter (\(Attendee x' y' _) -> (x-x')^2 + (y-y')^2 <= (d+10)^2)
    preferedInstrs :: (Double, Double) -> [Attendee] -> [(Instrument, Like)]
    preferedInstrs (x, y) = sortBy descByLike . zip [0..] . VG.toList . summary
      where descByLike x y = compare (snd y) (snd x)
            n = VG.length (tastes (VG.head (attendees prob)))
            summary :: [Attendee] -> VU.Vector Like -- この場所で期待できる楽器ごとのインパクト
            summary = foldr (\(Attendee x' y' ts) acc -> VG.zipWith (calc x' y') acc ts) (VG.replicate n 0.0)
              where
                calc :: Double -> Double -> Double -> Double -> Double
                calc x' y' acc t = acc + t / ((x-x')^2 + (y-y')^2)

-- | とりあえず Instrument と Like のリストを全部は使いこなせないので先頭だけ使うバージョン
--  Like で昇順ソートするので foldr で右から処理する想定
simpleStandingPositions ::  (Double, [Attendee]) -> Problem -> [((Double, Double), Instrument, Like)]
simpleStandingPositions xs prob
  = sortBy (\(_,_,l1) (_,_,l2) -> compare l1 l2) $ map (\(pos, (i, l):_) -> (pos, i, l)) res
  where res = standingPositions xs prob

musicianDictionary :: Problem -> Map.Map Instrument [Int]
musicianDictionary prob = Map.fromList $ splitWithOrder instrs ms
  where
    ms = zip [0..] (VG.toList (musicians prob))
    instrs = [0..n-1]
    n = VG.length (tastes (VG.head (attendees prob)))

splitWithOrder :: [Instrument] -> [(Int, Instrument)] -> [(Instrument, [Int])]
splitWithOrder instrs ms = map (\instr -> (instr, m Map.! instr)) instrs
  where
    seed = Map.fromList $ map (,[]) instrs
    m = foldl (\m' (i, instr) -> Map.insertWith (++) instr [i] m') seed ms

getCandidates :: Problem -> Either String [((Double, Double), Double)]
getCandidates prob = if Map.null ms'
                     then Right . map snd . sortBy (\x y -> compare (fst x) (fst y)) . Map.toList $ resp
                     else Left $ "error" ++ show (Map.elems ms')
  where
    (d, atnds) = decideFrontHeat 0.1 prob
    poss = standingPositions (d, atnds) prob
    ms = musicianDictionary prob
    (ms', resp) = foldr f (ms, Map.empty) poss
      where
        f :: ((Double, Double), [(Instrument, Like)])
          -> (Map.Map Instrument [Int], Map.Map Int ((Double, Double), Double))
          -> (Map.Map Instrument [Int], Map.Map Int ((Double, Double), Double))
        f (pos, (i, l):is) (ms, rs)
          | Map.null ms = (ms, rs)
          | otherwise = case ms Map.!? i of
              Nothing -> f (pos, is) (ms, rs)
              Just [] -> f (pos, is) (ms, rs)
              Just (j:[]) -> (Map.delete i ms,    Map.insert j (pos, volume) rs)
              Just (j:js) -> (Map.insert i js ms, Map.insert j (pos, volume) rs)
          where volume = if l < 0.0 then 0.0 else 10.0
