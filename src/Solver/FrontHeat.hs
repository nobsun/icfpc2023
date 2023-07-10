module Solver.FrontHeat where

import Data.List (sortBy, foldl')
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
frontAttendees d prob = filter inHeatArea (attendees prob)
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

-- | ステージ上の立ち位置を返す
--   (width, height)      ステージのサイズ
--   (setbackX, setbackY) ステージ端から離れる必要のある距離
--   (offsetX, offsetY)   ステージの左下の座標
trianglePositioning :: Problem -> [(Double, Double)]
trianglePositioning prob = map (\(x, y) -> (x + offsetX, y + offsetY)) $ filter inRange pos
  where
    (width, height) = (stage_width prob, stage_height prob)
    (setbackX, setbackY) = (10.0, 10.0)
    (offsetX, offsetY) = (stage_left prob, stage_bottom prob)
    
    inRange (x, y) = setbackX <= x && x <= width - setbackX && setbackY <= y && y <= height - setbackY
    hs, ws :: Int
    hs = round $ (height - 2 * setbackY) / 5.0
    ws = round $ (width - 2 * setbackX) / (10 * sqrt 3)
    pos = [conv (x, y) | x <- [0..ws], y <- [0..hs]]
      where
        -- sqrt 3 だと誤差のためミュージシャンが too closed になるので sqrt 3.1 にしている
        conv (x, y) = ( setbackX + 10 * sqrt 3.1 * fromIntegral x + if odd y then 5 * sqrt 3.1 else 0
                      , setbackY + 5 * fromIntegral y)

latticePositioning :: Problem -> [(Double, Double)]
latticePositioning prob = [ (x, y)
                          | x <- [w+10.0, w+20.0 .. e-10.0]
                          , y <- [s+10.0, s+20.0 .. n-10.0]
                          , x <= e-10.0 && y <= n-10.0 --Double の誤差のため
                          ]
  where
    (w, n, e, s) = stageBounds prob

data Formation
  = Lattice
  | Triangle
  deriving (Show, Eq)

-- | ステージ上の立てる場所を返す
--   それぞれの場所について、その場所にいる人たちの好みの楽器を高い順に並べる
--   Like がインパクトなので、Like が高い楽器を優先的に配置したい
standingPositions :: Formation -> (Double, [Attendee]) -> Problem -> [((Double, Double), [(Instrument, Like)])]
standingPositions fmt (d, atnds) prob
  = sortBy (\(_, (_, l1):_) (_, (_, l2):_) -> compare l2 l1)
    $ map (\pos -> (pos, preferedInstrs pos (near pos atnds))) poss
  where
    (w, n, e, s) = stageBounds prob
    poss = trianglePositioning prob
    near (x, y) = filter (\(Attendee x' y' _) -> (x-x')^2 + (y-y')^2 <= (d+10)^2)
    preferedInstrs :: (Double, Double) -> [Attendee] -> [(Instrument, Like)]
    preferedInstrs (x, y) = sortBy descByLike . zip [0..] . summary
      where descByLike x y = compare (snd y) (snd x)
            n = length (tastes (head (attendees prob)))
            summary :: [Attendee] -> [Like] -- この場所で期待できる楽器ごとのインパクト
            summary = foldr (\(Attendee x' y' ts) acc -> zipWith (calc x' y') acc ts) (replicate n 0.0)
              where
                calc :: Double -> Double -> Double -> Double -> Double
                calc x' y' acc t = acc + t / ((x-x')^2 + (y-y')^2)

musicianDictionary :: Problem -> Map.Map Instrument [Int]
musicianDictionary prob = Map.fromList $ splitWithOrder instrs ms
  where
    ms = zip [0..] (musicians prob)
    instrs = [0..n-1]
    n = length (tastes (head (attendees prob)))

splitWithOrder :: [Instrument] -> [(Int, Instrument)] -> [(Instrument, [Int])]
splitWithOrder instrs ms = map (\instr -> (instr, m Map.! instr)) instrs
  where
    seed = Map.fromList $ map (,[]) instrs
    m = foldl (\m' (i, instr) -> Map.insertWith (++) instr [i] m') seed ms

getCandidates :: Formation -> SolverF
getCandidates fmt prob = if Map.null ms'
                         then Right . map snd . sortBy (\x y -> compare (fst x) (fst y)) . Map.toList $ resp
                         else Left $ "error" ++ show (Map.elems ms')
  where
    (d, atnds) = decideFrontHeat 0.1 prob
    poss = standingPositions fmt (d, atnds) prob
    ms = musicianDictionary prob
    (ms', resp) = foldr f (ms, Map.empty) poss
      where
        f :: ((Double, Double), [(Instrument, Like)])
          -> (Map.Map Instrument [Int], Map.Map Int (Double, Double))
          -> (Map.Map Instrument [Int], Map.Map Int (Double, Double))
        f (pos, (i, _):is) (ms, rs)
          | Map.null ms = (ms, rs)
          | otherwise = case ms Map.!? i of
              Nothing -> f (pos, is) (ms, rs)
              Just [] -> f (pos, is) (ms, rs)
              Just (j:[]) -> (Map.delete i ms,    Map.insert j pos rs)
              Just (j:js) -> (Map.insert i js ms, Map.insert j pos rs)
