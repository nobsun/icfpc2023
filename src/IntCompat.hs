module IntCompat where

real :: RealFrac a => a -> Bool
real d = d == fromIntegral (floor d :: Int)

double :: Double -> Bool
double = real

float :: Float -> Bool
float = real
