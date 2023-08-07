module Postprocess.TuneVolume
  ( tuneVolume
  ) where

import qualified Data.IntMap (IntMap)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Vector.Generic as VG

import Answer (Answer)
import qualified Answer
import Problem (Problem)
import qualified Problem
import Extra (Extra)
import qualified Happiness


tuneVolume :: Extra -> Problem -> Answer -> Answer
tuneVolume extra prob ans
  = ans
  { Answer.volumes = Just $ VG.fromList $
      [ if IntMap.findWithDefault 0 k table > 0 then 10 else 0
      | k <- [0..n-1]
      ]
  }
  where
    n = VG.length (Problem.musicians prob)
    ans0 = ans{ Answer.volumes = Nothing {- Just (replicate n 1.0) -} }
    table = IntMap.fromListWith (+) [(k, val)  | (_i, k, val) <- Happiness.naiveUnreduced extra prob ans0]
