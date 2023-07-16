{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE RecordWildCards #-}

import qualified Data.Text as T
import qualified Data.Text.IO as T
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Options.Applicative
import Servant.Client (ClientEnv, mkClientEnv, parseBaseUrl)

import ICFPC2023System.API as API

data Options
  = Options
  { optProblemNo :: Maybe Int
  , optOffset :: Int
  , optLimit :: Int
  }

optionsParser :: Parser Options
optionsParser = Options
  <$> problemNo
  <*> offset
  <*> limit
  where
    problemNo :: Parser (Maybe Int)
    problemNo = option auto
      $  short 'p'
      <> long "problem"
      <> metavar "PROBLEM_NO"
      <> help "problem number"

    offset :: Parser Int
    offset = option auto
      $  long "offset"
      <> metavar "INT"
      <> help "offset"
      <> value 0
      <> showDefault

    limit :: Parser Int
    limit = option auto
      $  long "limit"
      <> metavar "INT"
      <> help "limit"
      <> value 100
      <> showDefault

parserInfo :: ParserInfo Options
parserInfo = info (optionsParser <**> helper)
  $  fullDesc
  <> header "get-submissions"

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

  submissions <- API.callICFPC2023System clientEnv $ do
    getSubmissions auth (Just (optOffset opt)) (Just (optLimit opt)) (optProblemNo opt)

  print submissions
