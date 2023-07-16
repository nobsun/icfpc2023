{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE RecordWildCards #-}

import qualified Data.Text as T
import qualified Data.Text.IO as T
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Options.Applicative
import Servant.Client (ClientEnv, mkClientEnv, parseBaseUrl)

import ICFPC2023System.API as API
import ICFPC2023System.Types as Types

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
    problemNo :: Parser Int
    problemNo = option auto
      $  short 'p'
      <> long "problem"
      <> metavar "PROBLEM_NO"
      <> help "problem number"

    solutionFile :: Parser FilePath
    solutionFile = argument str
      $  metavar "FILE"
      <> help "solution file"

parserInfo :: ParserInfo Options
parserInfo = info (optionsParser <**> helper)
  $  fullDesc
  <> header "submit"

main :: IO ()
main = do
  opt <- execParser parserInfo

  url <- parseBaseUrl "https://api.icfpcontest.com/"
  manager <- newManager tlsManagerSettings
  let clientEnv = mkClientEnv manager url

  token <- (head . T.lines) <$> T.readFile "token.txt"
  let auth = clientAuth token

  -- Create the client (all endpoint functions will be available)
  let ICFPC2023SystemBackend{..} = API.createICFPC2023SystemClient

  sol <- T.readFile (optSolutionFile opt)

  submission_id <- API.callICFPC2023System clientEnv $ do
    let req =
          Types.SubmissionRequest
          { Types.submissionRequestProblemUnderscoreid = optProblemNo opt
          , Types.submissionRequestContents = sol
          }
    postSubmission auth req

  print submission_id
