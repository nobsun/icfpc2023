{-# LANGUAGE DeriveGeneric #-}
module PastSubmissions where

import GHC.Generics
import Data.List
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.String (fromString)
import Data.Aeson
import System.Process (readProcess)

data PastScore
  = ScoreProcessing
  | ScoreSuccess Int
  | ScoreFailure String
  deriving (Eq, Show, Generic)

instance ToJSON PastScore where
  toJSON ScoreProcessing = String $ fromString "Processing"
  toJSON x = genericToJSON (tagPrefixOptions "Score") x

instance FromJSON PastScore where
  parseJSON (String s)
    | s == fromString "Processing" = pure ScoreProcessing
    | otherwise                    = fail $ "PastScore: unknown JSON string: " ++ show s
  parseJSON  x = genericParseJSON (tagPrefixOptions "Score") x

data PastSubmission =
  PastSubmission
  { _id :: String
  , problem_id :: Int
  , user_id :: String
  , score :: PastScore
  , submitted_at :: String
  }
  deriving (Eq, Show, Generic)

instance ToJSON PastSubmission
instance FromJSON PastSubmission

data PastSubmissions
  = SubmissionsSuccess [PastSubmission]
  | SubmissionsFailure String
  deriving (Eq, Show, Generic)

instance ToJSON PastSubmissions where
  toJSON = genericToJSON (tagPrefixOptions "Submissions")

instance FromJSON PastSubmissions where
  parseJSON = genericParseJSON (tagPrefixOptions "Submissions")

unPastSubmissions :: PastSubmissions -> [PastSubmission]
unPastSubmissions pss = case pss of
  SubmissionsSuccess xs -> xs
  SubmissionsFailure _  -> []

successScores :: PastSubmissions -> [Int]
successScores pss = [ v | ScoreSuccess v <- map score $ unPastSubmissions pss ]

customOption :: Options
customOption = defaultOptions { sumEncoding = ObjectWithSingleField }

tagPrefixOptions :: String -> Options
tagPrefixOptions prefix = customOption { constructorTagModifier = stripPrefix' prefix }

-- labelPrefixOptions :: String -> Options
-- labelPrefixOptions prefix = defaultOptions { fieldLabelModifier = stripPrefix' prefix }

stripPrefix' :: Eq a => [a] -> [a] -> [a]
stripPrefix' prefix s = maybe s id $ stripPrefix prefix s

---

getSubmissions :: Int -> Int-> IO (Maybe PastSubmissions)
getSubmissions probNum limit = do
  out <- readProcess "./api-sh/get-submissions.sh" [show probNum, show limit] ""
  return $ decode $ fromString out

---

_testsD :: [Maybe PastSubmission]
_testsD = map (decode . fromString) [_exampleS, _exampleF]

_printDE :: IO ()
_printDE = mapM_ (print . fmap L8.unpack . de . fromString) [_exampleS, _exampleF]
  where de s = do
          t <- decode s
          return $ encode (t :: PastSubmission)

testDED :: String -> Maybe Bool
testDED s0 = do
  t0 <- decode $ fromString s0
  let s1 = encode (t0 :: PastSubmission)
  t1 <- decode s1
  return $ t0 == t1

_testsDED :: [Maybe Bool]
_testsDED = map testDED [_exampleS, _exampleF]

_exampleS :: String
_exampleS =
    " { \
    \   \"_id\": \"64aa7ef53483a4efa0f0740d\",     \
    \   \"problem_id\": 2,                         \
    \   \"user_id\": \"64a3a7de8c685313d1c60d9d\", \
    \   \"score\": {                               \
    \     \"Success\": 120717346                   \
    \   },                                         \
    \   \"submitted_at\": \"2023-07-09T09:33:41.126480652Z\"  \
    \ }"

_exampleF :: String
_exampleF =
    " { \
    \   \"_id\": \"64a9cb7c3483a4efa0f00193\",        \
    \   \"problem_id\": 2,                            \
    \   \"user_id\": \"64a3a7de8c685313d1c60d9d\",    \
    \   \"score\": {                                  \
    \     \"Failure\": \"Musician is too close from the edge\"  \
    \   },                                            \
    \   \"submitted_at\": \"2023-07-08T20:47:56.778309996Z\"  \
    \ }"

_testGet :: Int -> IO ()
_testGet i = mapM_ print . maybe [] unPastSubmissions =<< getSubmissions i 3
