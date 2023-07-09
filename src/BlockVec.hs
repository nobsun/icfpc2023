module BlockVec where

-- |
-- isValidAnswer が通っていることを想定. 線分の端点を検査しない.
isBlock :: (Num a, Ord a) => (a, a) -> (a, a) -> (a, a) -> a -> Bool
isBlock m a b r =
  closer seg b r && cross lv m a
  where
    seg@(snv, _) = line2 m a  {- 音楽家と聴衆を結ぶ線分の直線 -}
    lv = along snv b {- ブロッカー b を通る垂線 -}
{-# SPECIALIZE isBlock :: (Double, Double) -> (Double, Double) -> (Double, Double) -> Double -> Bool #-}
{-# SPECIALIZE isBlock :: (Int, Int) -> (Int, Int) -> (Int, Int) -> Int -> Bool #-}

-- |
-- invalid である answer 用に、線分の端点も検査する
isBlockWithoutValid :: (Num a, Ord a) => (a, a) -> (a, a) -> (a, a) -> a -> Bool
isBlockWithoutValid m a b r =
  isBlock m a b r ||
  square2 (b |-| m) <= r*r ||  {- 線分の端点が半径 5 に含まれる -}
  square2 (b |-| a) <= r*r     {- 線分の端点が半径 5 に含まれる -}
{-# SPECIALIZE isBlockWithoutValid :: (Double, Double) -> (Double, Double) -> (Double, Double) -> Double -> Bool #-}
{-# SPECIALIZE isBlockWithoutValid :: (Int, Int) -> (Int, Int) -> (Int, Int) -> Int -> Bool #-}


-- |
-- normal v は v の法線ベクトル nv を返す
normal :: Num a => (a, a) -> (a, a)
normal (vx, vy) = (-vy, vx)  {- 法線ベクトル: xy を入れ替えて片方の符号を反転 -}
{-# SPECIALIZE normal :: (Double, Double) -> (Double, Double) #-}
{-# SPECIALIZE normal :: (Int, Int) -> (Int, Int) #-}

{- 以下、(nv, p) を、
   法線ベクトル nv を持ち、点 p を通る直線とする -}

-- |
-- along v q は
-- ベクトル v に沿った点 q を通る直線の
-- 法線ベクトル nv と直線上の点 q を返す
--
-- >>> along (1.0, 1.0) (1.0, 2.0)
-- ((-1.0,1.0),(1.0,2.0))
-- >>> along (1.0, -2.0) (1.0, 2.0)
-- ((2.0,1.0),(1.0,2.0))
along :: Num a => (a, a) -> (a, a) -> ((a, a), (a, a))
along v q = (normal v, q)
{-# SPECIALIZE along :: (Double, Double) -> (Double, Double) -> ((Double, Double), (Double, Double)) #-}
{-# SPECIALIZE along :: (Int, Int) -> (Int, Int) -> ((Int, Int), (Int, Int)) #-}

-- |
-- line2 (mx, my) (ax, ay) は
-- 2点 (mx, my) と (ax, ay) を通る直線の
-- 法線ベクトル nv と直線上の点 (mx, my) を返す
--
-- >>> line2 (0.0, 1.0) (1.0, 2.0)
-- ((1.0,-1.0),(0.0,1.0))
-- >>> line2 (0.0, 0.0) (1.0, 2.0)
-- ((2.0,-1.0),(0.0,0.0))
-- >>> line2 (0.0, 0.0) (2.0, 1.0)
-- ((1.0,-2.0),(0.0,0.0))
-- >>> line2 (-1.0, 1.0) (0.0, 3.0)
-- ((2.0,-1.0),(-1.0,1.0))
line2 :: Num a => (a, a) -> (a, a) -> ((a, a), (a, a))
line2 m@(mx, my) (ax, ay) = along v m
  where v = (mx - ax, my - ay)
{-# SPECIALIZE line2 :: (Double, Double) -> (Double, Double) -> ((Double, Double), (Double, Double)) #-}
{-# SPECIALIZE line2 :: (Int, Int) -> (Int, Int) -> ((Int, Int), (Int, Int)) #-}

-- |
-- closer (nv, p) q t は
-- 法線ベクトルを nv を持ち点 p を通る直線と、
-- 点 q の距離が t 以下であることを判定
closer :: (Num a, Ord a) => ((a, a), (a, a)) -> (a, a) -> a -> Bool
closer (nv, p) q t = lhs <= rhs
  where
    lhs = square ( nv |.| (p |-| q) )
    rhs = square2 nv * t * t
{-# SPECIALIZE closer :: ((Double, Double), (Double, Double)) -> (Double, Double) -> Double -> Bool #-}
{-# SPECIALIZE closer :: ((Int, Int), (Int, Int)) -> (Int, Int) -> Int -> Bool #-}
{-
   以下 (・) は内積

   | nv ・ (p - q) | ==
     {- 内積 -}
   |nv| * |p - q| * |cons α| ==
     {- 直線と q の距離を qd とすると、
        qd は法線 nv への正射影の長さとなり、 qd = |p - q| * |cons α|  -}
   |nv| * qd

   qd ≦ t  ⇔
     {- 両辺は正、|nv| を掛ける -}
   |nv| * qd ≦ |nv| * t  ⇔
     {- 両辺は正なので二乗してよい -}
   ( |nv| * qd )^2  ≦ |nv|^2 * t^2  ⇔
     {- |nv| * qd == | nv ・ (p - q) | であったので -}
   | nv ・ (p - q) |^2  ≦ |nv|^2 * t^2  {- 代わりにこの不等式を判定すれば良い -}
 -}

-- |
-- 直線の法線ベクトルを nv 直線上の点を p とする.
-- 2点 q, r を結ぶ線分が、直線と交差する.
cross :: (Num a, Ord a) => ((a, a), (a, a)) -> (a, a) -> (a, a) -> Bool
cross (nv, b) m a =
  (dp - nv |.| m) * (dp - nv |.| a) <= 0
  {-
     以下 (・) は内積

     音楽家と聴衆が垂線の両側. 内積値の符号が反転
     nv |.| (b |-| m) * nv |.| (b |-| a) <= 0
     nv |.| (b |-| m) == nv |.| b - nv |.| m
     nv |.| (b |-| a) == nv |.| b - nv |.| a
   -}
  where dp = nv |.| b
{-# SPECIALIZE cross :: ((Double, Double), (Double, Double)) -> (Double, Double) -> (Double, Double) -> Bool #-}
{-# SPECIALIZE cross :: ((Int, Int), (Int, Int)) -> (Int, Int) -> (Int, Int) -> Bool #-}

infixl 6 |-|
infix 7 |.|

(|-|) :: Num a => (a, a) -> (a, a) -> (a, a)
(px, py) |-| (qx, qy) = (px - qx, py - qy)
{-# SPECIALIZE (|-|) :: (Double, Double) -> (Double, Double) -> (Double, Double) #-}
{-# SPECIALIZE (|-|) :: (Int, Int) -> (Int, Int) -> (Int, Int) #-}

(|.|) :: Num a => (a, a) -> (a, a) -> a
(px, py) |.| (qx, qy) = px * qx + py * qy
{-# SPECIALIZE (|.|) :: (Double, Double) -> (Double, Double) -> Double #-}
{-# SPECIALIZE (|.|) :: (Int, Int) -> (Int, Int) -> Int #-}

square :: Num a => a -> a
square x = x * x
{-# SPECIALIZE square :: Double -> Double #-}
{-# SPECIALIZE square :: Int -> Int #-}

-- | 長さの二乗
square2 :: Num a => (a, a) -> a
square2 (x, y) = x * x + y * y
{-# SPECIALIZE square2 :: (Double, Double) -> Double #-}
{-# SPECIALIZE square2 :: (Int, Int) -> Int #-}
