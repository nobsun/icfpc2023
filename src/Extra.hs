{-# LANGUAGE RecordWildCards #-}
-- extra metadata for Problem and Answer
module Extra where

import qualified Data.Set as Set
import Text.Printf (printf)

import qualified IntCompat
import Problem
import Answer

data ProblemExtra
  = ProblemExtra { num_musicians :: Int
                 , num_attendees :: Int
                 , num_instruments :: Int
                 , problem_int_compat :: Bool
                 } deriving Show

mkProblemExtra :: Problem -> ProblemExtra
mkProblemExtra Problem{..} =
  ProblemExtra
  { num_musicians = length musicians
  , num_attendees = length attendees
  , num_instruments = Set.size is
  , problem_int_compat = all compatA attendees
  }
  where
    is = Set.fromList musicians
    compatA Attendee{..} = IntCompat.double x && IntCompat.double y

showProblemExtra :: Int -> ProblemExtra -> String
showProblemExtra i ProblemExtra{..} = unlines $ zipWith (++) tags bodies
  where
    tag = printf "%3d: " i
    spc = replicate (length tag) ' '
    tags = tag : repeat spc
    bodies =
      [ "musicians: " ++ show num_musicians
      , "attendees: " ++ show num_attendees
      , "instruments: " ++ show num_instruments
      , "int_compat: " ++ show problem_int_compat
      ]

printProblemExtras :: Int -> IO ()
printProblemExtras n =
  sequence_
  [ printExtra =<< readProblem i
  | i <- [1..n]
  , let printExtra = putStr . maybe ("parse error") (showProblemExtra i . mkProblemExtra)
  ]

data Extra
  = Extra { answer_valid :: Bool
          , answer_int_compat :: Bool
          , problem_extra :: ProblemExtra
          }

mkExtra' :: Problem -> ProblemExtra -> Answer -> Extra
mkExtra' problem pextra answer =
  Extra
  { answer_valid = isValidAnswer problem answer
  , answer_int_compat = isIntCompatAnswer answer
  , problem_extra = pextra
  }

mkExtra :: Problem -> Answer -> Extra
mkExtra problem = mkExtra' problem (mkProblemExtra problem)

data HappinessICompat
  = IntCompat
  | NotIntCompat
  deriving Show

int_compat_happiness :: Extra -> HappinessICompat
int_compat_happiness Extra{..}
  | answer_int_compat && problem_int_compat problem_extra  =  IntCompat
  | otherwise                                              =  NotIntCompat
