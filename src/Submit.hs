module Submit where

import Data.String (fromString)
import Data.Ord (comparing)
import Data.List (maximumBy)
import Data.Functor
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.Aeson
import System.Process (readProcess)

import Problem
import Answer
import Extra
import PastSubmissions
import Happiness
import Solutions

tryToSubmit :: Int -> [FilePath] -> IO ()
tryToSubmit _ [] = putStrLn "tryToSubmit: null input"
tryToSubmit probNum sols = do
  let readSol sol = do
        let parseError = putStrLn ("answer parse error: " ++ sol)
            result answer = pure [(answer, sol)]
        maybe (parseError $> []) result =<< readSolutionFile sol
  putStrLn $ "loading " ++ unwords sols
  answers <- concat <$> mapM readSol sols
  let action problem = tryToSubmit' probNum problem answers
  maybe (putStrLn $ "problem parse error: problem " ++ show probNum) action =<< readProblem probNum

tryToSubmit' :: Int -> Problem -> [(Answer, FilePath)] -> IO ()
tryToSubmit' probNum problem answers = do
  let calcH p@(ans, path) = do
        extra <- mkExtra problem ans
        let einfo = pprExtraShort extra
            strategy = Parallel
        putStrLn $ unwords [path ++ ":", "calulating", show strategy, "happiness:", einfo ]
        h <- Happiness.applyStrategy strategy extra problem ans
        pure (h, p)
  hs <- mapM calcH answers
  putStrLn $ "fetching past-max ..."
  pmax <- getPositiveMax probNum
  let (h, (answer, path)) = maximumBy (comparing fst) hs
      prefix = path ++ ": problem " ++ show probNum ++ ": past-max: " ++ show pmax ++ ", current-max: " ++ show h ++ " "
  if (h > pmax)
    then do
    putStrLn $ prefix ++ show h ++ " > " ++ show pmax ++ ", now to submit ..."
    submit probNum answer path
    else do
    putStrLn $ prefix ++ show h ++ " <= " ++ show pmax ++ ", skip to submit"

submit :: Int -> Answer -> FilePath -> IO ()
submit probNum answer path = do
  let answerStr = L8.unpack $ encode answer
      jkey = fromString
      submission = object [ jkey "problem_id" .= probNum, jkey "contents" .= answerStr ]
      submissionStr = L8.unpack $ encode submission
      receiveSubmissionId out = do
        let error' = putStrLn $ "fail to decode submission-id: output: " ++ out
        maybe error' (putStrLn . ("submission-id: " ++)) $ decode $ fromString out
  putStrLn $ path ++ ": submitting"
  receiveSubmissionId =<< readProcess "api-sh/submit-raw.sh" ["-x"] submissionStr

---

getPositiveMax :: Int -> IO Int
getPositiveMax probNum = do
  scores <- getSuccessScores probNum 10
  return $ maximum $ 0 : filter (> 0) scores

getSuccessScores :: Int -> Int -> IO [Int]
getSuccessScores probNum limit =
  maybe [] successScores <$> getSubmissions probNum limit
