import Data.Aeson (decode)
import qualified Data.ByteString.Lazy as BL
import Options.Applicative

import Answer
import Happiness
import Problem


data Options
  = Options
  { optProblemNo :: Int
  , optSolutionFile :: FilePath
  }

optionsParser :: Parser Options
optionsParser = Options
  <$> problemNo
  <*> solutionFile
  where
    problemNo = argument auto
      $  metavar "PROBLEM_NO"
      <> help "problem number"

    solutionFile :: Parser FilePath
    solutionFile = strArgument
      $  metavar "FILE"
      <> help "input filename"


parserInfo :: ParserInfo Options
parserInfo = info (optionsParser <**> helper)
  $  fullDesc
  <> header "eval"

main :: IO ()
main = do
  opt <- execParser parserInfo
  Just prob <- readProblem (optProblemNo opt)
  Just (ans :: Answer) <- decode <$> BL.readFile (optSolutionFile opt)
  putStrLn $ "score: " ++ show (happiness prob ans)
  putStrLn $ "valid: " ++ show (isValidAnswer prob ans)
