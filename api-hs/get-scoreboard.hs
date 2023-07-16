{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE RecordWildCards #-}

import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Servant.Client (ClientEnv, mkClientEnv, parseBaseUrl)

import ICFPC2023System.API as API

main :: IO ()
main = do
  url <- parseBaseUrl "https://api.icfpcontest.com/"
  manager <- newManager tlsManagerSettings
  let clientEnv = mkClientEnv manager url

  -- Create the client (all endpoint functions will be available)
  let ICFPC2023SystemBackend{..} = API.createICFPC2023SystemClient

  scoreboard <- API.callICFPC2023System clientEnv $ do
    getScoreboard
  print scoreboard
