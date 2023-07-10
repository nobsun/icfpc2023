module Main where

import Control.Monad
import Data.Aeson (decode, encode)
import qualified Data.ByteString.Lazy as BL
import Options.Applicative

import Answer
import qualified Extra
import Happiness
import Problem

import Postprocess.TuneVolume (tuneVolume)
import Postprocess.Swap (swap)


data Options
  = Options
  { optCommand :: String
  , optProblemNo :: Int
  , optInputFile :: FilePath
  , optOutputFile :: FilePath
  , optShowInputScore :: Bool
  , optShowOutputScore :: Bool
  , optSteps :: Maybe Int
  }

optionsParser :: Parser Options
optionsParser = Options
  <$> command
  <*> problemNo
  <*> inputFile
  <*> outputFile
  <*> showInputScore
  <*> showOutputScore
  <*> steps
  where
    problemNo = argument auto
      $  metavar "PROBLEM_NO"
      <> help "problem number"

    command :: Parser String
    command = strArgument
      $  metavar "COMMAND"
      <> help "command: tune-volume, swap"

    inputFile :: Parser FilePath
    inputFile = strArgument
      $  metavar "FILE"
      <> help "input solution file"

    outputFile :: Parser (FilePath)
    outputFile = strOption
      $  short 'o'
      <> metavar "FILE"
      <> help "output solution file"

    showInputScore :: Parser Bool
    showInputScore = flag False True
      $  long "show-input-score"
      <> help "show input score"

    showOutputScore :: Parser Bool
    showOutputScore = flag False True
      $  long "show-output-score"
      <> help "show output score"

    steps :: Parser (Maybe Int)
    steps = optional $ option auto
      $  long "steps"
      <> metavar "N"
      <> help "some command requires number of steps"

parserInfo :: ParserInfo Options
parserInfo = info (optionsParser <**> helper)
  $  fullDesc
  <> header "postprocess"

main :: IO ()
main = do
  opt <- execParser parserInfo
  Just prob <- readProblem (optProblemNo opt)
  Just sol1 <- decode <$> BL.readFile (optInputFile opt)

  when (optShowInputScore opt) $ do
    score1 <- happiness prob sol1
    putStrLn $ "input score: " ++ show score1

  (sol2, mScore2) <-
    case optCommand opt of
      "tune-volume" -> do
        extra <- Extra.mkExtra prob sol1
        pure (tuneVolume extra prob sol1, Nothing)
      "swap" -> do
        extra <- Extra.mkExtra prob sol1
        swap extra prob sol1 (optSteps opt)
      name -> error ("unknown command: " ++ name)

  BL.writeFile (optOutputFile opt) (encode sol2)

  when (optShowOutputScore opt) $ do
    score2 <-
      case mScore2 of
        Just s -> pure s
        Nothing -> happiness prob sol2
    putStrLn $ "output score: " ++ show score2
