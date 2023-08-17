{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
module Answer
  ( Placement (..)
  , Answer (placements, volumes)
  , mkAnswer
  , normalVolumes
  , isValidAnswer
  , isIntCompatAnswer
  ) where

import Data.String (fromString)
import qualified Data.ByteString.Lazy as B
import Data.Aeson
import qualified Data.Vector as V
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Unboxed as VU
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
  = Answer { placements :: V.Vector Placement
           , volumes    :: Maybe (VU.Vector Double)
           }
  deriving (Show, Eq, Generic)

mkAnswer :: [Placement] -> [Double] -> Answer
mkAnswer ps vs = Answer { placements = VG.fromList ps, volumes = fmap VG.fromList (normalVolumes vs) }

normalVolumes :: [Double] -> Maybe [Double]
normalVolumes vs
  | null vs = Nothing
  | otherwise = Just vs

instance ToJSON Placement
instance FromJSON Placement
instance ToJSON Answer where
  toJSON = genericToJSON defaultOptions { omitNothingFields = True }
instance FromJSON Answer

_test :: (B.ByteString, B.ByteString, Answer, Answer)
_test = ( encode ans1
        , encode ans2
        , maybe (error "parse error: str1") id $ decode str1
        , maybe (error "parse error: str2") id $ decode str2
        )
  where ans1 = Answer { placements = VG.fromList
                                     [ Placement {x = 10.0, y = 20.0}
                                     , Placement {x = 30.0, y = 40.0}
                                     ]
                      , volumes = Nothing
                      }
        ans2 = Answer { placements = VG.fromList
                                     [ Placement {x = 10.0, y = 20.0}
                                     , Placement {x = 30.0, y = 40.0}
                                     ]
                      , volumes = Just (VG.fromList [1.0, 2.0])
                      }

        str1 = fromString "{\"placements\":[{\"x\":10,\"y\":20},{\"x\":30,\"y\":40}]}"
        str2 = fromString "{\"placements\":[{\"x\":10,\"y\":20},{\"x\":30,\"y\":40}],\"volumes\":[1.0,2.0]}"

isValidAnswer :: Problem -> Answer -> Bool
isValidAnswer Problem{ stage_bottom_left = (x0,y0), stage_width = w, stage_height = h, musicians = ms } Answer{ placements = ps, volumes = _vs } =
  and
  [ length ps == VG.length ms
  , and [x0 + 10 <= x && x <= x0 + w - 10 && y0 + 10 <= y && y <= y0 + h - 10 | Placement x y <- VG.toList ps]
  , and [(x1 - x2)^(2::Int) + (y1 - y2)^(2::Int) >= 100 | (Placement x1 y1, Placement x2 y2) <- pairs' (VG.toList ps)]
  ]

pairs' :: [a] -> [(a,a)]
pairs' [] = []
pairs' (x:xs) = [(x,y) | y <- xs] ++ pairs' xs

isIntCompatAnswer :: Answer -> Bool
isIntCompatAnswer ans = all compatP $ placements ans
  where compatP pl = IntCompat.double (x pl) && IntCompat.double (y pl)
