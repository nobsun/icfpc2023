{-# LANGUAGE OverloadedLists #-}

module Solver.Rectangle where

import Data.List (sortBy, partition)
import qualified Data.Map as Map
import qualified Data.Vector.Generic as VG

import Problem
import Solver (SolverF)
import BlockVec (isBlock)

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

inRangeOfWestMusician :: Point -> Point -> Bool
inRangeOfWestMusician (x0, y0) (x, y) =
  (y - y0) + sqrt 3 * (x - x0) <= 0 &&
  (y - y0) - sqrt 3 * (x - x0) >= 0

inRangeOfNorthMusician :: Point -> Point -> Bool
inRangeOfNorthMusician (x0, y0) (x, y) =
  sqrt 3 * (y - y0) + (x - x0) >= 0 &&
  sqrt 3 * (y - y0) - (x - x0) >= 0

inRangeOfEastMusician :: Point -> Point -> Bool
inRangeOfEastMusician (x0, y0) (x, y) =
  (y - y0) + sqrt 3 * (x - x0) >= 0 &&
  (y - y0) - sqrt 3 * (x - x0) <= 0

inRangeOfSouthMusician :: Point -> Point -> Bool
inRangeOfSouthMusician (x0, y0) (x, y) =
  sqrt 3 * (y - y0) + (x - x0) <= 0 &&
  sqrt 3 * (y - y0) - (x - x0) <= 0

inRangeOfNorthWestMusician :: Point -> Point -> Bool
inRangeOfNorthWestMusician (x0, y0) (x, y) =
  sqrt 3 * (y - y0) - (x - x0) >= 0 &&
  (y - y0) - sqrt 3 * (x - x0) >= 0

inRangeOfNorthEastMusician :: Point -> Point -> Bool
inRangeOfNorthEastMusician (x0, y0) (x, y) =
  sqrt 3 * (y - y0) + (x - x0) >= 0 &&
  (y - y0) + sqrt 3 * (x - x0) >= 0

inRangeOfSouthEastMusician :: Point -> Point -> Bool
inRangeOfSouthEastMusician (x0, y0) (x, y) =
  sqrt 3 * (y - y0) - (x - x0) <= 0 &&
  (y - y0) - sqrt 3 * (x - x0) <= 0

inRangeOfSouthWestMusician :: Point -> Point -> Bool
inRangeOfSouthWestMusician (x0, y0) (x, y) =
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

data Align = LeftTop
           | LeftBottom
           | RightTop
           | RightBottom
  deriving (Show, Eq, Ord)

-- | 立ち位置の配置をどちら寄せかにしたときに後ろが余るのでそれを調整するかどうか
type Adjust = Bool

positions :: Problem -> (Align, Adjust) -> [(Point, Direction)]
positions prob (align, adjust) = map withDirection poss
  where
    withDirection p@(x, y)
      | adjust = arrange (p, direction p)
      | otherwise = (p, direction p)
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

    -- | NOTE: 雑に調整している。無くてもよいが加点が期待できるかもしれないのでやっただけ
    -- 左上から右下に向かって列挙したので西端と南端ステージ端に寄り切れてない
    -- うまく調整したいが今のところ西端と南端の最終列および最終行のみ調整する
    arrange :: (Point, Direction) -> (Point, Direction)
    arrange ((x, y), West)      = ((w+10.0, y),      West)
    arrange ((x, y), North)     = ((x,      n-10.0), North)
    arrange ((x, y), East)      = ((e-10.0, y),      East)
    arrange ((x, y), South)     = ((x,      s+10.0), South)
    arrange ((x, y), NorthWest) = ((w+10.0, n-10.0), NorthWest)
    arrange ((x, y), NorthEast) = ((e-10.0, n-10.0), NorthEast)
    arrange ((x, y), SouthEast) = ((e-10.0, s+10.0), SouthEast)
    arrange ((x, y), SouthWest) = ((w+10.0, s+10.0), SouthWest)
    arrange ((x, y), d)         = ((x,      y),      d)

    (w, n, e, s) = stageBounds prob
    -- 左上寄せ
    possLeftTop
      = [ (x, y)
        | x <- [w+10.0, w+20.0 .. e-10.0]
        , y <- [n-10.0, n-20.0 .. s+10.0]
        , x <= e-10.0 && y >= s+10.0 -- Double なのでこれがないと誤差でステージに近すぎる場合が出る
        ]
    -- 左下寄せ
    possLeftBottom
      = [ (x, y)
        | x <- [w+10.0, w+20.0 .. e-10.0]
        , y <- [s+10.0, s+20.0 .. n-10.0]
        , x <= e-10.0 && y <= n-10.0 -- Double なのでこれがないと誤差でステージに近すぎる場合が出る
        ]
    -- 右上寄せ
    possRightTop
      = [ (x, y)
        | x <- [e-10.0, e-20.0 .. w+10.0]
        , y <- [n-10.0, n-20.0 .. s+10.0]
        , x >= w+10.0 && y >= s+10.0 -- Double なのでこれがないと誤差でステージに近すぎる場合が出る
        ]
    -- 右下寄せ
    possRightBottom
      = [ (x, y)
        | x <- [e-10.0, e-20.0 .. w+10.0]
        , y <- [s+10.0, s+20.0 .. n-10.0]
        , x >= w+10.0 && y <= n-10.0 -- Double なのでこれがないと誤差でステージに近すぎる場合が出る
        ]
    --  ここでいろいろ試す
    poss = case align of
      LeftTop     -> possLeftTop
      LeftBottom  -> possLeftBottom
      RightTop    -> possRightTop
      RightBottom -> possRightBottom

    westEdge = minimum $ map fst poss
    eastEdge = maximum $ map fst poss
    northEdge = maximum $ map snd poss
    southEdge = minimum $ map snd poss

positionsByDirection :: Problem
                     -> (Align, Adjust)
                     -> ( [(Point, Direction)] -- West
                        , [(Point, Direction)] -- North
                        , [(Point, Direction)] -- East
                        , [(Point, Direction)] -- South
                        , [(Point, Direction)] -- NorthWest
                        , [(Point, Direction)] -- NorthEast
                        , [(Point, Direction)] -- SouthEast
                        , [(Point, Direction)] -- SouthWest
                        , [(Point, Direction)] -- Inner
                        )
positionsByDirection prob (align, adjust) = dividePoss poss
  where
    poss = positions prob (align, adjust)
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

attendeesForPositions :: Problem -> (Align, Adjust) -> Map.Map (Point, Direction) [Attendee]
attendeesForPositions prob (align, adjust)
  = Map.fromList
    $ westAtnds ++ northAtnds ++ eastAtnds ++ southAtnds
    ++ northWestAtnds ++ northEastAtnds ++ southEastAtnds ++ southWestAtnds
    ++ innerAtnds
  where
    atnds = attendees prob
    (westPoss, northPoss, eastPoss, southPoss
      , northWestPos, northEastPos, southEastPos, southWestPos
      , innerPoss) = positionsByDirection prob (align, adjust)
    westAtnds :: [((Point, Direction), [Attendee])]
    westAtnds = map (by inRangeOfWestMusician) westPoss
    northAtnds :: [((Point, Direction), [Attendee])]
    northAtnds = map (by inRangeOfNorthMusician) northPoss
    eastAtnds :: [((Point, Direction), [Attendee])]
    eastAtnds = map (by inRangeOfEastMusician) eastPoss
    southAtnds :: [((Point, Direction), [Attendee])]
    southAtnds = map (by inRangeOfSouthMusician) southPoss
    northWestAtnds :: [((Point, Direction), [Attendee])]
    northWestAtnds = map (by inRangeOfNorthWestMusician) northWestPos
    northEastAtnds :: [((Point, Direction), [Attendee])]
    northEastAtnds = map (by inRangeOfNorthEastMusician) northEastPos
    southEastAtnds :: [((Point, Direction), [Attendee])]
    southEastAtnds = map (by inRangeOfSouthEastMusician) southEastPos
    southWestAtnds :: [((Point, Direction), [Attendee])]
    southWestAtnds = map (by inRangeOfSouthWestMusician) southWestPos
    by pred pd@(p, _) = (pd, filter (\(Attendee x y _) -> pred p (x, y)) (VG.toList atnds))
    -- NOTE: 必ず外周から埋めるのでここが影響を与えることはない
    -- 外周に穴が開いているならここにはミュージシャンは配置されていないはず
    innerAtnds :: [((Point, Direction), [Attendee])]
    innerAtnds = map (\pd@(p, _) -> (pd, [])) innerPoss

-- | 柱を正確に拾うのは大変なのでざっくり拾う
--   方針として柱の中心と半径から柱に外接する正方形を作り、それに対して範囲に含まれるか判断する
pillarsForPositions :: Problem -> (Align, Adjust) -> Map.Map (Point, Direction) [Pillar]
pillarsForPositions prob (align, adjust)
  = Map.fromList $ westPlrs ++ northPlrs ++ eastPlrs ++ southPlrs
    ++ northWestPlrs ++ northEastPlrs ++ southEastPlrs ++ southWestPlrs
    ++ innerPlrs
  where
    plrs :: [Pillar]
    plrs = VG.toList $ pillars prob
    westPoss :: [(Point, Direction)]
    (westPoss, northPoss, eastPoss, southPoss
      , northWestPos, northEastPos, southEastPos, southWestPos
      , innerPoss) = positionsByDirection prob (align, adjust)
    westPlrs :: [((Point, Direction), [Pillar])]
    westPlrs = map f westPoss
      where f pd@(p, _)
              = (pd, filter (\(Pillar (x, y) r) -> inRangeOfWestMusician p (x+r, y+r) ||
                                                   inRangeOfWestMusician p (x+r, y-r)) plrs)
    northPlrs :: [((Point, Direction), [Pillar])]
    northPlrs = map f northPoss
      where f pd@(p, _)
              = (pd, filter (\(Pillar (x, y) r) -> inRangeOfNorthMusician p (x+r, y-r) ||
                                                   inRangeOfNorthMusician p (x-r, y-r)) plrs)
    eastPlrs :: [((Point, Direction), [Pillar])]
    eastPlrs = map f eastPoss
      where f pd@(p, _)
              = (pd, filter (\(Pillar (x, y) r) -> inRangeOfEastMusician p (x-r, y+r) ||
                                                   inRangeOfEastMusician p (x-r, y-r)) plrs)
    southPlrs :: [((Point, Direction), [Pillar])]
    southPlrs = map f southPoss
      where f pd@(p, _)
              = (pd, filter (\(Pillar (x, y) r) -> inRangeOfSouthMusician p (x+r, y+r) ||
                                                   inRangeOfSouthMusician p (x-r, y+r)) plrs)
    northWestPlrs :: [((Point, Direction), [Pillar])]
    northWestPlrs = map f northWestPos
      where f pd@(p, _)
              = (pd, filter (\(Pillar (x, y) r) -> inRangeOfNorthWestMusician p (x+r, y-r)) plrs)
              
    northEastPlrs :: [((Point, Direction), [Pillar])]
    northEastPlrs = map f northEastPos
      where f pd@(p, _)
              = (pd, filter (\(Pillar (x, y) r) -> inRangeOfNorthEastMusician p (x-r, y-r)) plrs)

    southEastPlrs :: [((Point, Direction), [Pillar])]
    southEastPlrs = map f southEastPos
      where f pd@(p, _)
              = (pd, filter (\(Pillar (x, y) r) -> inRangeOfSouthEastMusician p (x-r, y+r)) plrs)

    southWestPlrs :: [((Point, Direction), [Pillar])]
    southWestPlrs = map f southWestPos
      where f pd@(p, _)
              = (pd, filter (\(Pillar (x, y) r) -> inRangeOfSouthWestMusician p (x+r, y+r)) plrs)
              
    -- NOTE: 必ず外周から埋めるのでここが影響を与えることはない
    -- 外周に穴が開いているならここにはミュージシャンは配置されていないはず
    -- 外周を埋めつくす前提でいるが、人数が足りない場合は反対側 (背中側) の効果を見落としている
    innerPlrs :: [((Point, Direction), [Pillar])]
    innerPlrs = map (\pd@(p, _) -> (pd, [])) innerPoss

expectHappiness :: Problem -> Point -> [Attendee] -> [Pillar] -> [Like]
expectHappiness prob (x0, y0) atnds plrs = foldr expect (replicate n 0.0) atnds
  where
    n = VG.length (tastes (VG.head (attendees prob)))
    expect (Attendee x y ts) acc = zipWith (calcHappiness (x, y)) acc (VG.toList ts)
      where calcHappiness (x, y) acc t
              | disturbedBy plrs = acc
              | otherwise        = acc + 1e6*t / ((x-x0)^2 + (y-y0)^2)
            disturbedBy plrs = any (\(Pillar (px, py) r) -> isBlock (x0, y0) (x, y) (px, py) r) plrs

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

-- | NOTE: Inner 以外はソートして立ち位置の優先度を考慮する。 Inner はほぼ効果がないので最後に付ける
--   同じ楽器のミュージシャンによるコラボ効果はもしかしたら Inner でも意味があったかもしれない
standingPositions :: Problem
                  -> (Align, Adjust) -- 立ち位置に配置をどちら寄せにするかと最後を調整するか
                  -> [((Point, Direction), [(Instrument, Like)])]
standingPositions prob (align, adjust) = sortBy comp nonInners ++ inners
  where
    comp :: ((Point, Direction), [(Instrument, Like)])
         -> ((Point, Direction), [(Instrument, Like)])
         -> Ordering
    comp (_, (_, l1):_) (_, (_, l2):_) = compare l2 l1
    poss :: Map.Map (Point, Direction) [Attendee]
    poss = attendeesForPositions prob (align, adjust)
    plrs :: Map.Map  (Point, Direction) [Pillar]
    plrs = pillarsForPositions prob (align, adjust)
    exps :: Map.Map (Point, Direction) [(Instrument, Like)]
    exps = Map.mapWithKey f poss
      where
        f :: (Point, Direction) -> [Attendee] -> [(Instrument, Like)]
        f pd@(p, _) as = prefer $ expectHappiness prob p as ps
          where ps = plrs Map.! pd
        prefer :: [Like] -> [(Instrument, Like)]
        prefer ls = sortBy (\x y -> compare (snd y) (snd x)) (zip [0..] ls)
    (nonInners, inners) = partition (\((_, d), _) -> d /= Inner) $ Map.toList exps

getCandidates :: (Align, Adjust) -> SolverF
getCandidates (align, adjust) prob =
  pure $ if Map.null ms'
         then Right . map snd . sortBy (\x y -> compare (fst x) (fst y)) . Map.toList $ resp
         else Left $ "error" ++ show (Map.elems ms')
  where
    -- NOTE: foldr で末尾から処理しているので reverse しています
    poss = reverse $ standingPositions prob (align, adjust)
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
