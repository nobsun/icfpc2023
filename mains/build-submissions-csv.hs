{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Monad
import qualified Data.Aeson as J
import qualified Data.ByteString.Lazy as BL
import qualified Data.Csv as Csv
import Data.List (sortBy)
import Data.Maybe
import Data.Ord
import qualified Data.Text as T
import System.IO
import Text.Printf
import qualified Turtle
import qualified ICFPC2023System.Types as Types
import qualified PastSubmissions


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
  hPutStrLn stderr "Loading submissions"

  xs <- fmap concat $ forM [(1::Int)..90] $ \i -> do
    hPutStrLn stderr $ "problem " ++ show i
    fnames <- Turtle.sort $ Turtle.find (Turtle.suffix ".json") (printf "submissions/p%02d" i)
    fmap catMaybes $ forM fnames $ \fname -> do
      hPutStrLn stderr fname
      m <- J.decodeFileStrict' fname
      pure $ do
        sub <- Types.submissionResponseSuccess =<< m
        let info = Types.submissionSubmission sub
        score <- case J.fromJSON (Types.submissionInfoScore info) of
                   J.Error _ -> Nothing
                   J.Success n -> Just n
        pure $
          Row
          { rowId = Types.submissionInfoUnderscoreid info
          , rowProblemId = Types.submissionInfoProblemUnderscoreid info
          , rowUserId = Types.submissionInfoUserUnderscoreid info
          , rowFilePath = fname
          , rowScore =
              case score of
                PastSubmissions.ScoreSuccess n -> Just n
                _ -> Nothing
          , rowError =
              case score of
                PastSubmissions.ScoreFailure err -> Just (T.pack err)
                _ -> Nothing
          , rowSubmittedAt = Types.submissionInfoSubmittedUnderscoreat info
          }

  BL.writeFile "submissions.csv" $ Csv.encodeDefaultOrderedByName $ sortBy (comparing rowSubmittedAt) xs
