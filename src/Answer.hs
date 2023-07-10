{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
module Answer
  ( Placement (..)
  , Answer (..)
  , isValidAnswer
  , isIntCompatAnswer
  ) where

import Data.String (fromString)
import qualified Data.ByteString.Lazy as B
import Data.Aeson
-- import Text.Printf (printf)
import GHC.Generics

import qualified IntCompat
import Problem hiding (x, y)

data Placement
  = Placement { x :: Double
              , y :: Double
              }
  deriving (Show, Eq, Generic)

instance Obstacle Placement where
  obCenter a = (x a, y a)
  obRadius _ = 5

data Answer
  = Answer { placements :: [Placement]
           , volumes    :: [Double]
           }
  deriving (Show, Eq, Generic)

instance ToJSON Placement
instance FromJSON Placement
instance ToJSON Answer
instance FromJSON Answer

_test :: (B.ByteString, Answer)
_test = (encode ans, maybe (error "parse error!!") id $ decode str)
  where ans = Answer { placements = [ Placement {x = 10.0, y = 20.0}
                                    , Placement {x = 30.0, y = 40.0}
                                    ]
                     , volumes = [1.0, 2.0]
                     }

        str = fromString "{\"placements\":[{\"x\":10,\"y\":20},{\"x\":30,\"y\":40}]}"

isValidAnswer :: Problem -> Answer -> Bool
isValidAnswer Problem{ stage_bottom_left = (x0,y0), stage_width = w, stage_height = h, musicians = ms } Answer{ placements = ps, volumes = _vs } =
  and
  [ length ps == length ms
  , and [x0 + 10 <= x && x <= x0 + w - 10 && y0 + 10 <= y && y <= y0 + h - 10 | Placement x y <- ps]
  , and [(x1 - x2)^(2::Int) + (y1 - y2)^(2::Int) >= 100 | (Placement x1 y1, Placement x2 y2) <- pairs' ps]
  ]

pairs' :: [a] -> [(a,a)]
pairs' [] = []
pairs' (x:xs) = [(x,y) | y <- xs] ++ pairs' xs

isIntCompatAnswer :: Answer -> Bool
isIntCompatAnswer ans = all compatP $ placements ans
  where compatP pl = IntCompat.double (x pl) && IntCompat.double (y pl)
