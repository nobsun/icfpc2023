{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
module Problem where

import Control.Monad
import qualified Data.ByteString.Lazy as B
import Data.Aeson
import Text.Printf (printf)
import GHC.Generics

type Instrument = Int
type Like = Float

data Attendee
  = Attendee { x      :: Float
             , y      :: Float
             , tastes :: [Like]
             }
  deriving (Show, Eq, Generic)

data Problem
  = Problem { room_width        :: Float
            , room_height       :: Float
            , stage_width       :: Float
            , stage_height      :: Float
            , stage_bottom_left :: [Float]
            , musicians         :: [Instrument]
            , attendees         :: [Attendee]
            }
  deriving (Show, Eq, Generic)

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
    (left, bottom) <- case stage_bottom_left of
      left : bottom : _ -> return (left, bottom)
      _ -> Left ["unknown stage_bottom_left array: " ++ show stage_bottom_left]

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
