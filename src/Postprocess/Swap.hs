module Postprocess.Swap
  ( swap
  ) where

import Control.Exception
import Control.Monad
import qualified Data.Array.IArray as IArray
import qualified Data.IntMap (IntMap)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import Data.IORef
import qualified Data.Vector as V
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Unboxed as VU
import qualified System.Random.MWC as Rand
import qualified System.Random.MWC.Distributions as Rand
import Text.Printf

import Answer (Answer)
import qualified Answer
import Problem (Problem)
import qualified Problem
import Extra (Extra)
import qualified Extra
import Happiness (Happiness)
import qualified Happiness


swap :: Extra -> Problem -> Answer -> Maybe Int -> IO (Answer, Maybe Happiness)
swap extra prob ans maxIters
  | IntSet.size (IntSet.fromList (Problem.musicians prob)) <= 1 = return (ans, Nothing)
  | otherwise = do
      let instToMusicians :: V.Vector (VU.Vector Int)
          instToMusicians = V.fromList $ map VU.fromList $ IArray.elems (Extra.same_inst_musicians $ Extra.problem_extra extra)

          weights :: VU.Vector Double
          weights = VG.convert $ V.map (fromIntegral . VU.length) $ instToMusicians
      
      let ans0 = ans{ Answer.volumes = Nothing }
          table = IntMap.fromListWith IntSet.union [(k, IntSet.singleton i) | (i, k, _) <- Happiness.naiveUnreduced extra prob ans0]
      
      gen <- Rand.createSystemRandom
      h <- Happiness.happiness prob ans
      bestRef <- newIORef (ans, h)
      
      let loop !i
            | Just n <- maxIters, i >= n = return ()
            | otherwise = do
                (bestSol, bestHappiness) <- readIORef bestRef
                inst1 <- Rand.categorical weights gen
                inst2 <- Rand.categorical (weights VG.// [(inst1, 0)]) gen
                let table1 = instToMusicians VG.! inst1
                    table2 = instToMusicians VG.! inst2
                k1 <- (table1 VG.!) <$> Rand.uniformR (0, VU.length table1 - 1) gen
                k2 <- (table2 VG.!) <$> Rand.uniformR (0, VU.length table2 - 1) gen
                printf "try to swap %d (inst %d) and %d (inst %d) \n"  k1 inst1 k2 inst2
                -- TODO: 関係のあるattendeeだけに着目したproblemを作ることで計算を高速化
                -- let relevantAttendees = IntSet.union (table IntMap.! k1) (table IntMap.! k1)
                let swapElem :: [a] -> [a]
                    swapElem xs = V.toList $ xs' VG.// [(k1, xs' V.! k2), (k2, xs' V.! k1)]
                      where xs' = V.fromList xs
                    newSol =
                      bestSol
                      { Answer.placements = swapElem (Answer.placements bestSol)
                      , Answer.volumes = fmap swapElem (Answer.volumes bestSol)
                      }
                newHappiness <- Happiness.happiness prob newSol
                when (newHappiness > bestHappiness) $ do
                  printf "solution improved: %d -> %d\n" bestHappiness newHappiness
                  writeIORef bestRef (newSol, newHappiness)

                loop (i+1)

      catchJust (\e -> guard (e == UserInterrupt) >> pure ()) (loop 0) (\_ -> return ())
      (newSol, newHappiness) <- readIORef bestRef
      return (newSol, Just newHappiness)
