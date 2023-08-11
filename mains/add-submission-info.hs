{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Monad
import qualified Data.Aeson as J
import Data.List (sortBy)
import Data.Maybe
import Data.Ord
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import System.IO
import Text.Printf
import qualified Turtle
import qualified ICFPC2023System.Types as Types
import qualified Answer


main :: IO ()
main = do
  hPutStrLn stderr "Loading submissions"
  submissions <- loadSubmissions

  hPutStrLn stderr "Loading solutions"
  let pat = Turtle.suffix (Turtle.char '_' *> Turtle.decimal <* ".json")
  sols <- Turtle.sort (Turtle.find pat "solutions")
  forM_ sols $ \fname -> do
    let [probNo] = Turtle.match pat (T.pack fname)
    print fname
    b <- Turtle.testfile (fname ++ ".submit")
    unless b $ do
      Just (ans :: Answer.Answer) <- J.decodeFileStrict' fname
      case sortBy (comparing Types.submissionInfoSubmittedUnderscoreat)
           [ info
           | sub <- submissions
           , let info = Types.submissionSubmission sub
           , probNo == Types.submissionInfoProblemUnderscoreid info
           , Just ans == J.decodeStrict (T.encodeUtf8 (Types.submissionContents sub))
           ] of
        info : _ -> do
          writeFile (fname ++ ".submit") $ show (Types.submissionInfoUnderscoreid info) ++ "\n"
          return ()
        [] -> return ()

loadSubmissions :: IO [Types.Submission]
loadSubmissions = do
  fmap concat $ forM [(1::Int)..90] $ \i -> do
    hPutStrLn stderr $ "problem " ++ show i
    fnames <- Turtle.sort $ Turtle.find (Turtle.suffix ".json") (printf "submissions/p%02d" i)
    fmap catMaybes $ forM fnames $ \fname -> do
      hPutStrLn stderr fname
      m <- J.decodeFileStrict' fname
      pure $! Types.submissionResponseSuccess =<< m
