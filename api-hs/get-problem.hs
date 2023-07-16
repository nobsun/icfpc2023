{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE RecordWildCards #-}

import qualified Data.Text.IO as T
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Options.Applicative
import Servant.Client (ClientEnv, mkClientEnv, parseBaseUrl)
import System.Exit
import System.IO

import qualified ICFPC2023System.API as API
import qualified ICFPC2023System.Types as Types


data Options
  = Options
  { optProblemNo :: Int
  , optOutputFile :: Maybe FilePath
  }

optionsParser :: Parser Options
optionsParser = Options
  <$> problemNo
  <*> outputFile
  where
    problemNo = argument auto
      $  metavar "PROBLEM_NO"
      <> help "problem number"

    outputFile :: Parser (Maybe FilePath)
    outputFile = optional $ strOption
      $  short 'o'
      <> metavar "FILE"
      <> help "output filename"

parserInfo :: ParserInfo Options
parserInfo = info (optionsParser <**> helper)
  $  fullDesc
  <> header "get-problem"

main :: IO ()
main = do
  opt <- execParser parserInfo

  url <- parseBaseUrl "https://api.icfpcontest.com/"
  manager <- newManager tlsManagerSettings
  let clientEnv = mkClientEnv manager url

  -- Create the client (all endpoint functions will be available)
  let API.ICFPC2023SystemBackend{..} = API.createICFPC2023SystemClient

  resp <- API.callICFPC2023System clientEnv $ do
    getProblem (Just (optProblemNo opt))

  case Types.problemResponseFailure resp of
    Nothing -> return ()
    Just msg -> do
      T.hPutStrLn stderr msg
      exitFailure

  case Types.problemResponseSuccess resp of
    Nothing -> undefined
    Just prob -> do
      case optOutputFile opt of
        Nothing -> T.putStrLn prob
        Just fname -> T.writeFile fname prob
