
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
  let path = printf "solutions/%s_%03d.json" name probNum
  putStrLn $ "writing " ++ path
  B.writeFile path $ encode ans
