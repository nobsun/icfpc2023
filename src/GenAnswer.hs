
module GenAnswer where

import qualified Data.ByteString.Lazy as B
import Data.Aeson (encode)
import Text.Printf (printf)

import Solver

saveAnswer :: (Name, SolverF)
           -> Int
           -> IO ()
saveAnswer (name, solver) probNum = do
  ans <- solve' name solver probNum
  let path = printf "solutions/name_%03d.json" probNum
  B.writeFile path $ encode ans
