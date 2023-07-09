{-# LANGUAGE RecordWildCards #-}
-- extra metadata for Problem and Answer
module Extra where

import Data.Array (Array)
import Data.Array.IArray ((!), listArray)
import Data.Array.Unboxed (UArray)
import Data.IORef (IORef, newIORef)
import Text.Printf (printf)

import qualified IntCompat
import Problem
import Answer

data ProblemExtra
  = ProblemExtra { num_musicians :: Int
                 , num_attendees :: Int
                 , num_instruments :: Int
                 , attendees_int_compat :: Bool
                 , million_times_atnds_tastes :: Array Int (UArray Int Double)
                 } deriving Show

mkProblemExtra :: Problem -> ProblemExtra
mkProblemExtra Problem{..} =
  ProblemExtra
  { num_musicians = length musicians
  , num_attendees = length attendees
  , num_instruments = maximum (0 : musicians) + 1
  , attendees_int_compat = all compatA attendees
  , million_times_atnds_tastes = listArray (0, length attendees - 1) $ map million_times_tastes attendees
  }
  where
    compatA Attendee{..} = IntCompat.double x && IntCompat.double y

    {- each tastes times 1,000,000 memos -}
    million_times_tastes :: Attendee -> UArray Int Double
    million_times_tastes a = listArray (0, length ts - 1) $ map (1e6 *) ts  where ts = tastes a

pprProblemExtraShort :: ProblemExtra -> String
pprProblemExtraShort ProblemExtra{..} =
  unwords
  [ "musicians:" ++ show num_musicians
  , "attendees:" ++ show num_attendees
  , "instruments:" ++ show num_instruments
  , "attendee-int:" ++ if attendees_int_compat then "compat" else "not-compat"
  ]

pprProblemExtra :: Int -> ProblemExtra -> String
pprProblemExtra i ProblemExtra{..} = unlines $ zipWith (++) tags bodies
  where
    tag = printf "%3d: " i
    spc = replicate (length tag) ' '
    tags = tag : repeat spc
    bodies =
      [ "musicians: " ++ show num_musicians
      , "attendees: " ++ show num_attendees
      , "instruments: " ++ show num_instruments
      , "attendee-int:" ++ if attendees_int_compat then "compat" else "not-compat"
      ]

printProblemExtras :: Int -> IO ()
printProblemExtras n =
  sequence_
  [ printExtra =<< readProblem i
  | i <- [1..n]
  , let printExtra = putStr . maybe ("parse error") (pprProblemExtra i . mkProblemExtra)
  ]

data AnswerCheck
  = Valid
  | Invalid
  deriving Show

-- memory for calculating happiness, etc..
data S = S{}

data Extra
  = Extra { problem_extra :: ProblemExtra
          , answer_valid :: AnswerCheck
          , answer_int_compat :: Bool
          , state :: IORef S
          }

mkExtra' :: Problem -> ProblemExtra -> Answer -> IO Extra
mkExtra' problem pextra answer = do
  s <- newIORef S{}
  pure Extra
    { problem_extra = pextra
    , answer_valid = if isValidAnswer problem answer then Valid else Invalid
    , answer_int_compat = isIntCompatAnswer answer
    , state = s
    }

mkExtra :: Problem -> Answer -> IO Extra
mkExtra problem = mkExtra' problem (mkProblemExtra problem)

updateExtra :: Problem -> Answer -> Extra -> Extra
updateExtra problem answer extra =
  extra
  { answer_valid = if isValidAnswer problem answer then Valid else Invalid
  , answer_int_compat = isIntCompatAnswer answer
  }

data BlockTestICompat
  = IntCompat
  | NotIntCompat
  deriving Show

int_compat_blocktest :: Extra -> BlockTestICompat
int_compat_blocktest Extra{..}
  | answer_int_compat && attendees_int_compat problem_extra  =  IntCompat
  | otherwise                                              =  NotIntCompat

pprExtraShort :: Extra -> String
pprExtraShort e@Extra{..} =
  unwords
  [ pprProblemExtraShort problem_extra
  , "answer:" ++ show answer_valid
  , "answer-int:" ++ if answer_int_compat then "compat" else "not-compat"
  , "blocktest:" ++ show (int_compat_blocktest e)
  ]
