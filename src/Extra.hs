{-# LANGUAGE RecordWildCards #-}
-- extra metadata for Problem and Answer
module Extra where

import Data.Array.IArray (listArray)
import Data.Array.Unboxed (UArray)
import Data.Function (on)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.List (sortBy, groupBy)
import Data.IORef (IORef, newIORef)
import qualified Data.Vector as V
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Unboxed as VU
import Text.Printf (printf)

import qualified IntCompat
import Problem
import Answer

data Division
  = Lightening
  | Full
  deriving (Eq, Show)

data ProblemExtra
  = ProblemExtra { division :: Division
                 , num_musicians :: Int
                 , num_attendees :: Int
                 , num_instruments :: Int
                 , attendees_int_compat :: Bool
                 , pillars_int_compat :: Bool
                 , million_times_atnds_tastes :: V.Vector (VU.Vector Double)
                 , same_inst_musicians :: V.Vector [Int]
                 } deriving Show

mkProblemExtra :: Problem -> ProblemExtra
mkProblemExtra p@Problem{..} =
  ProblemExtra
  { division = if isFullDivisionProblem p then Full else Lightening
  , num_musicians = VG.length musicians
  , num_attendees = VG.length attendees
  , num_instruments = num_instruments
  , attendees_int_compat = all compatA attendees
  , pillars_int_compat = all compatP pillars
  , million_times_atnds_tastes = VG.map million_times_tastes attendees
  , same_inst_musicians = same_inst_musicians
  }
  where
    compatA Attendee{..} = IntCompat.double x && IntCompat.double y
    compatP Pillar{..} = IntCompat.double (fst center) && IntCompat.double (snd center) && IntCompat.double radius

    {- each tastes times 1,000,000 memos -}
    million_times_tastes :: Attendee -> VU.Vector Double
    million_times_tastes a = VU.map (1e6 *) ts  where ts = tastes a

    num_instruments = VG.maximum (VG.cons 0 musicians) + 1

    same_inst_musicians = V.fromList [IntMap.findWithDefault [] inst m | inst <- [0 .. num_instruments - 1]]
      where
        m = IntMap.fromListWith (++) $ zip (VG.toList musicians) $ map (\k -> [k]) [0..]


pprProblemExtraShort :: ProblemExtra -> String
pprProblemExtraShort e = unwords $ listProblemExtra e

pprProblemExtra :: Int -> ProblemExtra -> String
pprProblemExtra i e = unlines $ zipWith (++) tags $ listProblemExtra e
  where
    tag = printf "%3d: " i
    spc = replicate (length tag) ' '
    tags = tag : repeat spc

listProblemExtra :: ProblemExtra -> [String]
listProblemExtra ProblemExtra{..} =
  [ "musicians: " ++ show num_musicians
  , "attendees: " ++ show num_attendees
  , "instruments: " ++ show num_instruments
  , "attendee-int:" ++ if attendees_int_compat then "compat" else "not-compat"
  ] ++
  [ "pillars-int:" ++ if pillars_int_compat then "compat" else "not-compat" | Full <- [division] ]

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
data S = S
  { s_answer        :: Answer
  , s_m_m_distance  :: UArray Int Double
  , s_m_a_distance  :: UArray Int Double
  , s_closeness     :: UArray Int Double
  }

mkDummyS :: ProblemExtra -> S
mkDummyS pextra =
  S{ s_answer = mkAnswer (take (num_musicians pextra) (repeat (Placement(-1)(-1))))
         (take (num_musicians pextra) (repeat 1))
   , s_m_m_distance = listArray (0, sum[1..num_musicians pextra-1]) (repeat 0)
   , s_m_a_distance = listArray (0, (num_musicians pextra)*(num_attendees pextra)) (repeat 0)
   , s_closeness = listArray (0, num_musicians pextra) (repeat 0)
   }

mmIndex :: Int -> Int -> Int
mmIndex i j =
  sum[0..t-2]+b
  where
    (b,t) = (min i j, max i j)

maIndex :: Int -> Int -> Int -> Int
maIndex numMusician m a = a*numMusician+m


data Extra
  = Extra { problem_extra :: ProblemExtra
          , answer_valid :: AnswerCheck
          , answer_int_compat :: Bool
          , state :: IORef [(Int,S)]
          }

mkExtra' :: Problem -> ProblemExtra -> Answer -> IO Extra
mkExtra' problem pextra answer = do
  s <- newIORef []
  pure $ Extra
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
  | answer_int_compat, Full <- division, attendees_int_compat, pillars_int_compat  =  IntCompat
  | answer_int_compat, Lightening <- division, attendees_int_compat                =  IntCompat
  | otherwise                                                                      =  NotIntCompat
  where
    ProblemExtra{..} = problem_extra

pprExtraShort :: Extra -> String
pprExtraShort e@Extra{..} =
  unwords
  [ pprProblemExtraShort problem_extra
  , "answer:" ++ show answer_valid
  , "answer-int:" ++ if answer_int_compat then "compat" else "not-compat"
  , "blocktest:" ++ show (int_compat_blocktest e)
  ]
