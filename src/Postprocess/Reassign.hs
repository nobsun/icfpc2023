module Postprocess.Reassign
  ( reassignMusicians
  ) where

import qualified Data.Array.IArray as Array
import Data.Array.Unboxed (UArray)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap as IntMap
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import Data.Maybe
import qualified Data.Vector as V
import Data.Vector.Generic ((!))
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Unboxed as VU

import BipartiteMatching (maximumWeightPerfectMatchingComplete)

import qualified Answer
import qualified Extra
import qualified Happiness
import qualified Problem


-- | 解のミュージシャンの配置を入れ替える
--
-- 入れ替えは割り当て問題を解くことによって行われ、ボリュームは考慮されるが Playing Together 拡張は考慮されない。
-- Playing Together 拡張が存在しない状況では(現在の配置座標集合の元での)最適解を返す。
reassignMusicians
  :: Extra.Extra
  -> Problem.Problem
  -> Answer.Answer
  -> Answer.Answer
reassignMusicians extra prob answer = 
  case maximumWeightPerfectMatchingComplete xs xs f of
    (_obj, m, _) ->
      let placements' = V.fromList (Answer.placements answer)
       in answer{ Answer.placements = [placements' ! k | k <- IntMap.elems m] }
  where
    probExtra = Extra.problem_extra extra

    insts = VU.fromList (Problem.musicians prob)

    xs = IntSet.fromList [0 .. Extra.num_musicians probExtra - 1]

    f :: Int -> Int -> Int
    f k j = ceiling $ (vs ! k) * fromIntegral (impacts ! j ! (insts ! k))

    vs = maybe (VU.replicate (Extra.num_musicians probExtra) 1.0) VU.fromList (Answer.volumes answer)

    -- impacts ! k ! inst は k 番目の位置に楽器instのミュージシャンを配置した場合のインパクト
    impacts :: V.Vector (VU.Vector Int)
    impacts = VG.fromList
      [ VG.accum (+) (VG.replicate (Extra.num_instruments probExtra) 0)
          [ (inst, ceiling (num / den))
          | (i, a_i) <- zip [0..] (Problem.attendees prob)
          , and [not $ isBlock p_k a_i p_j | (j, p_j) <- zip [(0::Int)..] (Answer.placements answer), k /= j]
          , and [not $ isBlock p_k a_i pl | pl <- Problem.pillars prob]
          , let d = sqrt (Happiness.squareDistance p_k a_i)
          , let den = d*d -- なぜかsqrtして2乗するとジャッジに完全に一致するらしい
          , (inst, num) <- Array.assocs $ Extra.million_times_atnds_tastes probExtra Array.! i
          ]
      | (k, p_k) <- zip [0..] (Answer.placements answer)
      ]
    
    isBlock :: Problem.Obstacle o => Answer.Placement -> Problem.Attendee -> o -> Bool
    isBlock = Happiness.isBlockWith (Extra.int_compat_blocktest extra) (Extra.answer_valid extra)
