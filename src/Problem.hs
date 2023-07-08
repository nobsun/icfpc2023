{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
module Problem where

import Control.Monad
import qualified Data.ByteString.Lazy as B
import qualified Data.Set as Set
import Data.Aeson
import Text.Printf (printf)
import GHC.Generics

type Instrument = Int
type Like = Double

data Attendee
  = Attendee { x      :: Double
             , y      :: Double
             , tastes :: [Like]
             }
  deriving (Show, Eq, Generic)

data Problem
  = Problem { room_width        :: Double
            , room_height       :: Double
            , stage_width       :: Double
            , stage_height      :: Double
            , stage_bottom_left :: (Double, Double)
            , musicians         :: [Instrument]
            , attendees         :: [Attendee]
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

instance ToJSON Attendee
instance FromJSON Attendee
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
          [ "instruments id set inconsistent: " ++ show (Set.size is) ++ " /= " ++ show (maximum (0 : musicians) + 1)
          | let is = Set.fromList musicians, Set.size is /= maximum (0 : musicians) + 1 ]

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
