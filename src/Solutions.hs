module Solutions where

import qualified Data.ByteString.Lazy as B
import Data.Aeson (encode, decode)
import Text.Printf (printf)

import Answer
import Problem
import Solver
import Happiness

saveAnswer :: (Name, SolverF)
           -> Int
           -> IO ()
saveAnswer (name, solver) probNum = do
  (ans, _problem) <- solve' name solver probNum
  let path = printf "solutions/%s_%03d.json" name probNum
  putStrLn $ "writing " ++ path --- ++ " : happiness: " ++ show (happiness problem ans)
  B.writeFile path $ encode ans

readSolution :: Name -> Int -> IO (Maybe Answer)
readSolution name probNum = do
  let path = printf "solutions/%s_%03d.json" name probNum
  inp <- B.readFile path
  return $ decode inp

calcHappiness :: Name -> Int -> IO Happiness
calcHappiness name probNum = do
  let probMark = "problem " ++ show probNum
  problem <- maybe (fail $ "parse error: " ++ probMark) pure =<< readProblem probNum
  answer <- maybe (fail $ "parse error: " ++ probMark) pure =<<readSolution name probNum
  let path = printf "solutions/%s_%03d.json" name probNum
  putStrLn $ "calulating happiness: " ++ path
  return $! weightedAverageHappiness problem answer
