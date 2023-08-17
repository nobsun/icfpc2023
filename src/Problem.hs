{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
module Problem where

import Control.Monad
import qualified Data.ByteString.Lazy as B
import qualified Data.Set as Set
import Data.Aeson
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import Text.Printf (printf)
import GHC.Generics

type Instrument = Int
type Like = Double

data Attendee
  = Attendee { x      :: Double
             , y      :: Double
             , tastes :: VU.Vector Like
             }
  deriving (Show, Eq, Generic)

data Pillar
  = Pillar { center :: (Double, Double)
           , radius :: Double
           }
  deriving (Show, Eq, Generic)

class Obstacle a where
  obCenter :: a -> (Double, Double)
  obRadius :: a -> Double

instance Obstacle Attendee where
  obCenter a = (x a, y a)
  obRadius _ = 0

instance Obstacle Pillar where
  obCenter = center
  obRadius = radius

pillar_center_x :: Pillar -> Double
pillar_center_x Pillar{..} = case center of
  (x, _y) -> x

pillar_center_y :: Pillar -> Double
pillar_center_y Pillar{..} = case center of
  (_x, y) -> y

data Problem
  = Problem { room_width        :: Double
            , room_height       :: Double
            , stage_width       :: Double
            , stage_height      :: Double
            , stage_bottom_left :: (Double, Double)
            , musicians         :: VU.Vector Instrument
            , attendees         :: V.Vector Attendee
            , pillars           :: V.Vector Pillar
            }
  deriving (Show, Eq, Generic)

stage_left :: Problem -> Double
stage_left Problem{..} = case stage_bottom_left of
  (left, _bottom) -> left

stage_right :: Problem -> Double
stage_right Problem{..} = stage_left Problem{..} + stage_width

stage_bottom :: Problem -> Double
stage_bottom Problem{..} = case stage_bottom_left of
  (_left, bottom) -> bottom

stage_top :: Problem -> Double
stage_top Problem{..} = stage_bottom Problem{..} + stage_height

-- | Full division かどうかの判定
--
-- \"Playing Together\" を有効にするかの判定に利用することを想定。
-- 今のところ full division の問題では pillars が非空なので、それで判定。
isFullDivisionProblem :: Problem -> Bool
isFullDivisionProblem Problem{..} = not (null pillars)

instance ToJSON Attendee
instance FromJSON Attendee
instance ToJSON Pillar
instance FromJSON Pillar
instance ToJSON Problem
instance FromJSON Problem

readProblem :: Int -> IO (Maybe Problem)
readProblem q = do
  inp <- B.readFile (printf "problems/%03d.json" q)
  let problem = (decode inp :: Maybe Problem)
  return problem

checkProblem :: Problem -> Either [String] ()
checkProblem Problem{..} = do
    let (left, bottom) = stage_bottom_left

    let errors =
          [ "left < 0: " ++ show left ++ " < 0" | left < 0 ]
          ++
          [ "bottom < 0: " ++ show bottom ++ " < 0" | bottom < 0 ]
          ++
          [ "left + stage_width > room_width: " ++ show (left + stage_width) ++ " > " ++ show  room_width
          | left + stage_width > room_width ]
          ++
          [ "bottom + stage_height > room_height: " ++ show (bottom + stage_height) ++ " > " ++ show room_height
          | bottom + stage_height > room_height ]
          ++
          [ "instruments id set inconsistent: " ++ show (Set.size is) ++ " /= " ++ show (VU.maximum (VU.cons 0 musicians) + 1)
          | let is = Set.fromList (VU.toList musicians), Set.size is /= VU.maximum (VU.cons 0 musicians) + 1 ]

    when (length errors > 0) $ Left errors


printCheckProblems :: Int -> IO ()
printCheckProblems n =
    sequence_
    [ printResult
    | i <- [1..n]
    , let getResult = maybe ["parse error"] (either id (const ["good"]) . checkProblem) <$> readProblem i
          printResult = do
            ms <- getResult
            let tag = show i ++ ": "
                space = replicate (length tag) ' '
                xs = zipWith (++) (tag : repeat space) ms
            putStr $ unlines xs
    ]

centerOfStage :: Problem -> (Double, Double)
centerOfStage Problem{..} = (left + stage_width / 2, bottom + stage_height / 2)
  where
    (left, bottom) = stage_bottom_left
