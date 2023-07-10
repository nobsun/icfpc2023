module Solver.Rectangle where

import Data.List (sortBy, partition)
import qualified Data.Map as Map

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
--   * (y - y0) + sqrt 3 (x - x0) <= 0
--   * (y - y0) - sqrt 3 (x - x0) >= 0
--  北に配置されたミュージシャン (x0, y0) が影響を与える観客の条件:
--   * sqrt 3 (y - y0) + (x - x0) >= 0
--   * sqrt 3 (y - y0) - (x - x0) >= 0
--  東に配置されたミュージシャン (x0, y0) が影響を与える観客の条件:
--   * (y - y0) + sqrt 3 (x - x0) >= 0
--   * (y - y0) - sqrt 3 (x - x0) <= 0
--  南に配置されたミュージシャン (x0, y0) が影響を与える観客の条件:
--   * sqrt 3 (y - y0) + (x - x0) <= 0
--   * sqrt 3 (y - y0) - (x - x0) <= 0
--
-- 角にいるミュージシャンの影響を及ぼす範囲
--
--  北西に配置されたミュージシャン (x0, y0) が影響を与える観客の条件:
--   * sqrt 3 (y - y0) - (x - x0) >= 0
--   * (y - y0) - sqrt 3 (x - x0) >= 0
--  北東に配置されたミュージシャン (x0, y0) が影響を与える観客の条件:
--   * sqrt 3 (y - y0) + (x - x0) >= 0
--   * (y - y0) + sqrt 3 (x - x0) >= 0
--  南東に配置されたミュージシャン (x0, y0) が影響を与える観客の条件:
--   * sqrt 3 (y - y0) - (x - x0) <= 0
--   * (y - y0) - sqrt 3 (x - x0) <= 0
--  南西に配置されたミュージシャン (x0, y0) が影響を与える観客の条件:
--   * sqrt 3 (y - y0) + (x - x0) <= 0
--   * (y - y0) + sqrt 3 (x - x0) <= 0

inRangeOfWestMusician :: Point -> Attendee -> Bool
inRangeOfWestMusician (x0, y0) (Attendee x y _) =
  (y - y0) + sqrt 3 * (x - x0) <= 0 &&
  (y - y0) - sqrt 3 * (x - x0) >= 0

inRangeOfNorthMusician :: Point -> Attendee -> Bool
inRangeOfNorthMusician (x0, y0) (Attendee x y _) =
  sqrt 3 * (y - y0) + (x - x0) >= 0 &&
  sqrt 3 * (y - y0) - (x - x0) >= 0

inRangeOfEastMusician :: Point -> Attendee -> Bool
inRangeOfEastMusician (x0, y0) (Attendee x y _) =
  (y - y0) + sqrt 3 * (x - x0) >= 0 &&
  (y - y0) - sqrt 3 * (x - x0) <= 0

inRangeOfSouthMusician :: Point -> Attendee -> Bool
inRangeOfSouthMusician (x0, y0) (Attendee x y _) =
  sqrt 3 * (y - y0) + (x - x0) <= 0 &&
  sqrt 3 * (y - y0) - (x - x0) <= 0

inRangeOfNorthWestMusician :: Point -> Attendee -> Bool
inRangeOfNorthWestMusician (x0, y0) (Attendee x y _) =
  sqrt 3 * (y - y0) - (x - x0) >= 0 &&
  (y - y0) - sqrt 3 * (x - x0) >= 0

inRangeOfNorthEastMusician :: Point -> Attendee -> Bool
inRangeOfNorthEastMusician (x0, y0) (Attendee x y _) =
  sqrt 3 * (y - y0) + (x - x0) >= 0 &&
  (y - y0) + sqrt 3 * (x - x0) >= 0

inRangeOfSouthEastMusician :: Point -> Attendee -> Bool
inRangeOfSouthEastMusician (x0, y0) (Attendee x y _) =
  sqrt 3 * (y - y0) - (x - x0) <= 0 &&
  (y - y0) - sqrt 3 * (x - x0) <= 0

inRangeOfSouthWestMusician :: Point -> Attendee -> Bool
inRangeOfSouthWestMusician (x0, y0) (Attendee x y _) =
  sqrt 3 * (y - y0) + (x - x0) <= 0 &&
  (y - y0) + sqrt 3 * (x - x0) <= 0

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
  deriving (Show, Eq, Ord)

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

attendeesForPositions :: Problem -> Map.Map (Point, Direction) [Attendee]
attendeesForPositions prob
  = Map.fromList
    $ westAtnds ++ northAtnds ++ eastAtnds ++ southAtnds
    ++ northWestAtnds ++ northEastAtnds ++ southEastAtnds ++ southWestAtnds
    ++ innerAtnds
  where
    atnds = attendees prob
    poss = positions prob
    (westPoss, northPoss, eastPoss, southPoss
      , northWestPos, northEastPos, southEastPos, southWestPos
      , innerPoss) = dividePoss poss
    dividePoss = foldr divide ([], [], [], [], [], [], [], [], [])
      where
        divide p@(_, West) (ws, ns, es, ss, nws, nes, ses, sws, ins)
          = (p:ws, ns, es, ss, nws, nes, ses, sws, ins)
        divide p@(_, North) (ws, ns, es, ss, nws, nes, ses, sws, ins)
          = (ws, p:ns, es, ss, nws, nes, ses, sws, ins)
        divide p@(_, East) (ws, ns, es, ss, nws, nes, ses, sws, ins)
          = (ws, ns, p:es, ss, nws, nes, ses, sws, ins)
        divide p@(_, South) (ws, ns, es, ss, nws, nes, ses, sws, ins)
          = (ws, ns, es, p:ss, nws, nes, ses, sws, ins)
        divide p@(_, NorthWest) (ws, ns, es, ss, nws, nes, ses, sws, ins)
          = (ws, ns, es, ss, p:nws, nes, ses, sws, ins)
        divide p@(_, NorthEast) (ws, ns, es, ss, nws, nes, ses, sws, ins)
          = (ws, ns, es, ss, nws, p:nes, ses, sws, ins)
        divide p@(_, SouthEast) (ws, ns, es, ss, nws, nes, ses, sws, ins)
          = (ws, ns, es, ss, nws, nes, p:ses, sws, ins)
        divide p@(_, SouthWest) (ws, ns, es, ss, nws, nes, ses, sws, ins)
          = (ws, ns, es, ss, nws, nes, ses, p:sws, ins)
        divide p@(_, Inner) (ws, ns, es, ss, nws, nes, ses, sws, ins)
          = (ws, ns, es, ss, nws, nes, ses, sws, p:ins)
    westAtnds :: [((Point, Direction), [Attendee])]
    westAtnds = map (\pd@(p, _) -> (pd, filter (inRangeOfWestMusician p) atnds)) westPoss
    northAtnds :: [((Point, Direction), [Attendee])]
    northAtnds = map (\pd@(p, _) -> (pd, filter (inRangeOfNorthMusician p) atnds)) northPoss
    eastAtnds :: [((Point, Direction), [Attendee])]
    eastAtnds = map (\pd@(p, _) -> (pd, filter (inRangeOfEastMusician p) atnds)) eastPoss
    southAtnds :: [((Point, Direction), [Attendee])]
    southAtnds = map (\pd@(p, _) -> (pd, filter (inRangeOfSouthMusician p) atnds)) southPoss
    northWestAtnds :: [((Point, Direction), [Attendee])]
    northWestAtnds = map (\pd@(p, _) -> (pd, filter (inRangeOfNorthWestMusician p) atnds)) northWestPos
    northEastAtnds :: [((Point, Direction), [Attendee])]
    northEastAtnds = map (\pd@(p, _) -> (pd, filter (inRangeOfNorthEastMusician p) atnds)) northEastPos
    southEastAtnds :: [((Point, Direction), [Attendee])]
    southEastAtnds = map (\pd@(p, _) -> (pd, filter (inRangeOfSouthEastMusician p) atnds)) southEastPos
    southWestAtnds :: [((Point, Direction), [Attendee])]
    southWestAtnds = map (\pd@(p, _) -> (pd, filter (inRangeOfSouthWestMusician p) atnds)) southWestPos
    -- NOTE: 必ず外周から埋めるのでここが影響を与えることはない
    -- 外周に穴が開いているならここにはミュージシャンは配置されていないはず
    innerAtnds :: [((Point, Direction), [Attendee])]
    innerAtnds = map (\pd@(p, _) -> (pd, [])) innerPoss

-- | TODO
pillarsForPositions :: Problem -> Map.Map (Point, Direction) [Pillar]
pillarsForPositions prob = Map.empty


expectHappiness :: Point -> [Attendee] -> [Like]
expectHappiness (x0, y0) atnds = foldr expect (replicate n 0.0) atnds
  where
    n = length (tastes (head atnds))
    expect (Attendee x y ts) acc = zipWith (calcHappiness (x0, y0) (x, y)) acc ts
      where calcHappiness (x0, y0) (x, y) acc t = acc + 1e6*t / ((x-x0)^2 + (y-y0)^2)

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

-- | NOTE: Inner 以外はソートして立ち位置の優先度を考慮する。 Inner はほぼ効果がないので最後に付ける
standingPositions :: Problem -> [((Point, Direction), [(Instrument, Like)])]
standingPositions prob = sortBy comp nonInners ++ inners
  where
    comp :: ((Point, Direction), [(Instrument, Like)])
         -> ((Point, Direction), [(Instrument, Like)])
         -> Ordering
    comp (_, (_, l1):_) (_, (_, l2):_) = compare l2 l1
    poss = attendeesForPositions prob
    (nonInners, inners) = partition (\((_, d), _) -> d /= Inner) $ Map.toList exps
    exps :: Map.Map (Point, Direction) [(Instrument, Like)]
    exps = Map.mapWithKey f poss
      where
        f (p, _) as = prefer $ expectHappiness p as
        prefer :: [Like] -> [(Instrument, Like)]
        prefer ls = sortBy (\x y -> compare (snd y) (snd x)) (zip [0..] ls)

getCandidates :: SolverF
getCandidates prob =
  pure $
  if Map.null ms'
  then Right . map snd . sortBy (\x y -> compare (fst x) (fst y)) . Map.toList $ resp
  else Left $ "error" ++ show (Map.elems ms')
  where
    -- NOTE: foldr で末尾から処理しているので reverse しています
    poss = reverse $ standingPositions prob
    ms = musicianDictionary prob
    (ms', resp) = foldr f (ms, Map.empty) poss
      where
        f :: ((Point, Direction), [(Instrument, Like)])
          -> (Map.Map Instrument [Int], Map.Map Int (Point, Double))
          -> (Map.Map Instrument [Int], Map.Map Int (Point, Double))
        f ((pos, d), (i, l):is) (ms, rs)
          | Map.null ms = (ms, rs)
          | otherwise = case ms Map.!? i of
              Nothing -> f ((pos, d), is) (ms, rs)
              Just [] -> f ((pos, d), is) (ms, rs)
              Just (j:[]) -> (Map.delete i ms,    Map.insert j (pos, volume) rs)
              Just (j:js) -> (Map.insert i js ms, Map.insert j (pos, volume) rs)
          where volume = if l < 0.0 then 0.0 else 10.0


_sampleProblem :: Problem
_sampleProblem
  = Problem { room_width = 100.0
            , room_height = 100.0
            , stage_width = 80.0
            , stage_height = 80.0
            , stage_bottom_left = (10.0, 10.0)
            , musicians = []
            , attendees = [ Attendee { x = 5.0
                                     , y = 5.0
                                     , tastes = []
                                     }
                          , Attendee { x = 5.0
                                     , y = 50.0
                                     , tastes = []
                                     }
                          , Attendee { x = 5.0
                                     , y = 95.0
                                     , tastes = []
                                     }
                          , Attendee { x = 50.0
                                     , y = 5.0
                                     , tastes = []
                                     }
                          , Attendee { x = 50.0
                                     , y = 95.0
                                     , tastes = []
                                     }
                          , Attendee { x = 95.0
                                     , y = 5.0
                                     , tastes = []
                                     }
                          , Attendee { x = 95.0
                                     , y = 50.0
                                     , tastes = []
                                     }
                          , Attendee { x = 95.0
                                     , y = 95.0
                                     , tastes = []
                                     }
                          ]
            , pillars = []
            }
