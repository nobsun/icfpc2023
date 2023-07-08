{-# LANGUAGE NoOverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module SolverArraying where

import Problem
import Solver (SolverF)

type Cord = Float

data Range = Range { rgMin :: Cord, rgMax :: Cord }
instance Show Range where show (Range n x) = show (n, x)

mkRange :: Cord -> Cord -> Range
mkRange a b = Range (min a b) (max a b)

data Rect = Rect { rangeX :: Range, rangeY :: Range } deriving Show

xmin, xmax, ymin, ymax :: Rect -> Cord

xmin = rgMin . rangeX
xmax = rgMax . rangeX

ymin = rgMin . rangeY
ymax = rgMax . rangeY

---

getStageRect :: Problem -> Rect
getStageRect p@Problem{..} =
  Rect { rangeX = mkRange stage_left_ stage_right,   rangeY = mkRange stage_bottom_ stage_top }
  where
    stage_left_ = stage_left p
    stage_bottom_ = stage_bottom p
    stage_right = stage_left_ + stage_width
    stage_top = stage_bottom_ + stage_height

-- そもそもはみ出るというのは誤解だった - 問題 1 から 問題 55
-- ステージが部屋からは一方向にしかはみ出ていないという前提
-- ステージと部屋の共通部分の長方形と、もしあれば、部屋からはみ出ている長方形を返す
getStageRectsOld :: Problem -> Either String (Rect, Maybe Rect)
getStageRectsOld Problem{..} = case stage_bottom_left of
      left : bottom : _ -> result left bottom
      _ -> Left $ "getRects: unknown stage_bottom_left array: " ++ show stage_bottom_left
  where
    result stage_left stage_bottom
      | over_right =
        Right
        ( Rect { rangeX = mkRange stage_left room_width,   rangeY = mkRange stage_bottom stage_top } ,
          Just $
          Rect { rangeX = mkRange room_width stage_right,  rangeY = mkRange stage_bottom stage_top } )
      | over_top =
        Right
        ( Rect { rangeX = mkRange stage_left stage_right,  rangeY = mkRange stage_bottom room_height } ,
          Just $
          Rect { rangeX = mkRange stage_left stage_right,  rangeY = mkRange room_height stage_top } )
      | over_left =  {- いまのところ負の left は無さそうだが念のため -}
        Right
        ( Rect { rangeX = mkRange 0 stage_right,  rangeY = mkRange stage_bottom stage_top } ,
          Just $
          Rect { rangeX = mkRange stage_left 0,   rangeY = mkRange stage_bottom stage_top } )
      | over_bottom =  {- いまのところ負の bottom は無さそうだが念のため -}
        Right
        ( Rect { rangeX = mkRange stage_left stage_right,  rangeY = mkRange 0 stage_top } ,
          Just $
          Rect { rangeX = mkRange stage_left stage_right,  rangeY = mkRange stage_bottom 0 } )
      | and $ map (not . fst) overs =
        Right
        ( Rect { rangeX = mkRange stage_left stage_right,  rangeY = mkRange stage_bottom stage_top } ,
          Nothing )
      | otherwise =
        Left $ "getRects: not simple case, overs: " ++ showOvers
      where
        stage_right = stage_left + stage_width
        stage_top = stage_bottom + stage_height

        over_right = stage_right > room_width
        over_top = stage_top > room_height
        over_left = stage_left < 0
        over_bottom = stage_bottom < 0

        overs =
          [ (over_right, "right")
          , (over_top, "top")
          , (over_left, "left")
          , (over_bottom, "bottom")
          ]

        showOvers = unwords $ map snd $ filter fst overs

---

cordsFromRect :: Rect -> [(Cord, Cord)]
cordsFromRect Rect{..} = [ (x, y) | x <- cordsFromRange rangeX, y <- cordsFromRange rangeY ]

cordsFromRange :: Range -> [Cord]
cordsFromRange Range{..} = map fromIntegral [ rgMin' + 10, rgMin' + 20 .. rgMax' - 10 ]
  where
    rgMin' = round rgMin :: Int
    rgMax' = round rgMax :: Int

---

getCandidates :: SolverF
getCandidates problem = Right $ take len $ cordsFromRect stage
  where
    len = length $ musicians problem
    stage = getStageRect problem

getCandidatesOld :: SolverF
getCandidatesOld problem = do
  (r0, mr1) <- getStageRectsOld problem
  let len = length $ musicians problem
  return $ take len $ concatMap cordsFromRect $ maybe [r0] (\r1 -> [r0, r1]) mr1
