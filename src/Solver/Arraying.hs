{-# LANGUAGE NoOverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Solver.Arraying where

import qualified Data.Vector.Generic as VG

import Problem
import Solver (SolverF)

type Coord = Double

data Range = Range { rgMin :: Coord, rgMax :: Coord }
instance Show Range where show (Range n x) = show (n, x)

mkRange :: Coord -> Coord -> Range
mkRange a b = Range (min a b) (max a b)

data Rect = Rect { rangeX :: Range, rangeY :: Range } deriving Show

xmin, xmax, ymin, ymax :: Rect -> Coord

xmin = rgMin . rangeX
xmax = rgMax . rangeX

ymin = rgMin . rangeY
ymax = rgMax . rangeY

---

getStageRect :: Problem -> Rect
getStageRect p@Problem{..} =
  Rect { rangeX = mkRange left right,   rangeY = mkRange bottom top }
  where
    left = stage_left p
    bottom = stage_bottom p
    right = left + stage_width
    top = bottom + stage_height

-- そもそもはみ出るというのは誤解だった - 問題 1 から 問題 55
-- ステージが部屋からは一方向にしかはみ出ていないという前提
-- ステージと部屋の共通部分の長方形と、もしあれば、部屋からはみ出ている長方形を返す
getStageRectsOld :: Problem -> Either String (Rect, Maybe Rect)
getStageRectsOld Problem{..} = case stage_bottom_left of
      (left, bottom) -> result left bottom
  where
    result left bottom
      | over_right =
        Right
        ( Rect { rangeX = mkRange left room_width,   rangeY = mkRange bottom top } ,
          Just $
          Rect { rangeX = mkRange room_width right,  rangeY = mkRange bottom top } )
      | over_top =
        Right
        ( Rect { rangeX = mkRange left right,  rangeY = mkRange bottom room_height } ,
          Just $
          Rect { rangeX = mkRange left right,  rangeY = mkRange room_height top } )
      | over_left =  {- いまのところ負の left は無さそうだが念のため -}
        Right
        ( Rect { rangeX = mkRange 0 right,  rangeY = mkRange bottom top } ,
          Just $
          Rect { rangeX = mkRange left 0,   rangeY = mkRange bottom top } )
      | over_bottom =  {- いまのところ負の bottom は無さそうだが念のため -}
        Right
        ( Rect { rangeX = mkRange left right,  rangeY = mkRange 0 top } ,
          Just $
          Rect { rangeX = mkRange left right,  rangeY = mkRange bottom 0 } )
      | and $ map (not . fst) overs =
        Right
        ( Rect { rangeX = mkRange left right,  rangeY = mkRange bottom top } ,
          Nothing )
      | otherwise =
        Left $ "getRects: not simple case, overs: " ++ showOvers
      where
        right = left + stage_width
        top = bottom + stage_height

        over_right = right > room_width
        over_top = top > room_height
        over_left = left < 0
        over_bottom = bottom < 0

        overs =
          [ (over_right, "right")
          , (over_top, "top")
          , (over_left, "left")
          , (over_bottom, "bottom")
          ]

        showOvers = unwords $ map snd $ filter fst overs

---

cordsFromRect :: Rect -> [(Coord, Coord)]
cordsFromRect Rect{..} = [ (x, y) | x <- cordsFromRange rangeX, y <- cordsFromRange rangeY ]

cordsFromRange :: Range -> [Coord]
cordsFromRange Range{..} = map fromIntegral [ rgMin' + 10, rgMin' + 20 .. rgMax' - 10 ]
  where
    rgMin' = round rgMin :: Int
    rgMax' = round rgMax :: Int

---

getCandidates :: Problem -> Either String [((Double, Double), Double)]
getCandidates problem = Right $ map (\pos -> (pos, 1.0)) $ take len $ cordsFromRect stage -- FIXME
  where
    len = VG.length $ musicians problem
    stage = getStageRect problem

getCandidatesOld :: Problem -> Either String [((Double, Double), Double)]
getCandidatesOld problem = do
  (r0, mr1) <- getStageRectsOld problem
  let len = VG.length $ musicians problem
  -- FIXME
  return $ map (\pos -> (pos, 1.0)) $ take len $ concatMap cordsFromRect $ maybe [r0] (\r1 -> [r0, r1]) mr1
