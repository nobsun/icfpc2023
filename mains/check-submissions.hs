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
import qualified PastSubmissions


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
                    case parseScore score of
                      PastSubmissions.ScoreProcessing -> putStrLn "Processing"
                      PastSubmissions.ScoreFailure err -> putStrLn err
                      PastSubmissions.ScoreSuccess judge -> do
                        putStrLn $ "judge: " ++ show judge
                        case J.eitherDecodeStrict (T.encodeUtf8 contents) of
                          Left err -> error err
                          Right (answer :: Answer.Answer) -> do
                            local <- Happiness.happiness prob answer
                            putStrLn $ "local: " ++ show local
                            unless (judge == local) $ putStrLn "mismatch"


parseScore :: J.Value -> PastSubmissions.PastScore
parseScore (J.String "Processing") = PastSubmissions.ScoreProcessing
parseScore (J.Object m) =
  case KeyMap.lookup "Failure" m of
    Just (J.String err) -> PastSubmissions.ScoreFailure (T.unpack err)
    Just _ -> error "should not happen"
    Nothing -> do
      case KeyMap.lookup (fromString "Success") m of
        Just (J.Number val) -> PastSubmissions.ScoreSuccess (ceiling (val :: Scientific.Scientific))
        _ -> error "should not happen"
parseScore _ = error "should not happen"
