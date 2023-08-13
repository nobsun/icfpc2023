{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Monad
import qualified Data.Aeson as J
import qualified Data.ByteString.Lazy as BL
import qualified Data.Csv as Csv
import qualified Data.Map.Strict as Map
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Vector as V
import System.IO
import qualified Turtle

import qualified Answer
import qualified Happiness
import qualified Problem


data Row
  = Row
  { rowId :: T.Text
  , rowProblemId :: Int
  , rowUserId :: T.Text
  , rowSubmittedAt :: T.Text
  , rowScore :: Maybe Int
  , rowError :: Maybe T.Text
  , rowFilePath :: FilePath
  }

instance Csv.ToNamedRecord Row where
  toNamedRecord (Row _id problem_id user_id submitted_at score err filepath) = Csv.namedRecord
    [ "id" Csv..= _id
    , "problem_id" Csv..= problem_id
    , "user_id" Csv..= user_id
    , "submitted_at" Csv..= submitted_at
    , "score" Csv..= score
    , "error" Csv..= err
    , "filepath" Csv..= filepath
    ]

instance Csv.FromNamedRecord Row where
  parseNamedRecord m = Row
    <$> m Csv..: "id"
    <*> m Csv..: "problem_id"
    <*> m Csv..: "user_id"
    <*> m Csv..: "submitted_at"
    <*> m Csv..: "score"
    <*> m Csv..: "error"
    <*> m Csv..: "filepath"

instance Csv.DefaultOrdered Row where
  headerOrder _ = Csv.header ["id", "problem_id", "user_id", "submitted_at", "score", "error", "filepath"]


main :: IO ()
main = do
  hPutStrLn stderr "Loading problems"
  problems <- forM [(1::Int)..90] $ \i -> do
    hPutStrLn stderr $ "Problem " ++ show i
    fromJust <$> Problem.readProblem i

  hPutStrLn stderr "Loading submissions.csv"
  s <- BL.readFile "submissions.csv"
  let submissions = 
        case Csv.decodeByName s of
          Left e -> error e
          Right (_, rows) -> Map.fromList [(rowId row, row) | row <- V.toList rows]

  hPutStrLn stderr "Processing solutions"
  let pat = Turtle.suffix (Turtle.char '_' *> Turtle.decimal <* ".json")
  sols <- Turtle.sort (Turtle.find pat "solutions")

  forM_ sols $ \fname -> do
    hPutStrLn stderr fname
    let [probNo] = Turtle.match pat (T.pack fname)

    b <- Turtle.testfile (fname ++ ".submit")
    submissionId <-
      if b then do
        s <- readFile (fname ++ ".submit")
        return $ Just $ T.pack $ read s
      else do
        return Nothing

    b <- Turtle.testfile (fname ++ ".score")
    unless b $ do
      score <- do
        let m1 = do
              subId <- submissionId
              row <- Map.lookup subId submissions
              rowScore row
        case m1 of
          Just s -> return s
          Nothing -> do
            let prob = problems !! (probNo - 1)
            Just (ans :: Answer.Answer) <- J.decodeFileStrict' fname
            Happiness.happiness prob ans
      writeFile (fname ++ ".score") $ show score ++ "\n"
