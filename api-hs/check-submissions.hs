{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Monad
import qualified Data.Aeson as J
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Scientific as Scientific
import Data.String (fromString)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Text.Printf
import qualified Turtle
import qualified ICFPC2023System.Types as Types

import qualified Answer
import qualified Happiness
import qualified Problem


main :: IO ()
main = do
  forM_ [(1::Int)..90] $ \i -> do
    Just prob <- Problem.readProblem i

    Turtle.sh $ do
      fname <- Turtle.find (Turtle.suffix ".json") (printf "submissions/p%02d" i)
      Turtle.liftIO $ do
        print fname
        ret <- J.eitherDecodeFileStrict' fname
        case ret of
          Left err -> error err
          Right (x :: Types.SubmissionResponse) -> do
            case Types.submissionResponseFailure x of
              Just err -> print (T.unpack err)
              Nothing -> do
                case Types.submissionResponseSuccess x of
                  Nothing -> error "neither Failure or Success"
                  Just Types.Submission
                       { Types.submissionSubmission =
                           Types.SubmissionInfo
                           { Types.submissionInfoUnderscoreid = _submission_id
                           , Types.submissionInfoProblemUnderscoreid = _problem_id
                           , Types.submissionInfoUserUnderscoreid = _user_id
                           , Types.submissionInfoScore = score
                           }
                       , Types.submissionContents = contents
                       } -> do
                    case score of
                      J.String _processing -> return ()
                      J.Object m -> do
                        case KeyMap.lookup (fromString "Failure") m of
                          Just err -> do
                            print err
                          Nothing -> do
                            case KeyMap.lookup (fromString "Success") m of
                              Nothing -> error "shuold not happen"
                              Just (J.Number val) -> do
                                let judge :: Int
                                    judge = ceiling (val :: Scientific.Scientific)
                                putStrLn $ "judge: " ++ show judge
                                case J.eitherDecodeStrict (T.encodeUtf8 contents) of
                                  Left err -> error err
                                  Right (answer :: Answer.Answer) -> do
                                    local <- Happiness.happiness prob answer
                                    putStrLn $ "local: " ++ show local
                                    unless (judge == local) $ putStrLn "mismatch"
                              Just _ -> error "not a number"
                      _ -> error "shuold not happen"
