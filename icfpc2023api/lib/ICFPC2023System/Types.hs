{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# OPTIONS_GHC -fno-warn-unused-binds -fno-warn-unused-imports #-}

module ICFPC2023System.Types (
  JWTResponse (..),
  LoginRequest (..),
  ProblemResponse (..),
  ProblemsResponse (..),
  RegisterRequest (..),
  Scoreboard (..),
  ScoreboardItem (..),
  Submission (..),
  SubmissionInfo (..),
  SubmissionRequest (..),
  SubmissionResponse (..),
  SubmissionsResponse (..),
  UserboardResponse (..),
  UserboardResponseSuccess (..),
  ) where

import Data.Data (Data)
import Data.UUID (UUID)
import Data.List (lookup)
import Data.Maybe (fromMaybe)
import Data.Aeson (Value, FromJSON(..), ToJSON(..), genericToJSON, genericParseJSON)
import Data.Aeson.Types (Options(..), defaultOptions)
import Data.Set (Set)
import Data.Text (Text)
import Data.Time
import Data.Swagger (ToSchema, declareNamedSchema)
import qualified Data.Swagger as Swagger
import qualified Data.Char as Char
import qualified Data.Text as T
import qualified Data.Map as Map
import GHC.Generics (Generic)


-- | your JWT access token
data JWTResponse = JWTResponse
  { jWTResponseSuccess :: Maybe Text -- ^ your JWT access token
  , jWTResponseFailure :: Maybe Text -- ^ failure message
  } deriving (Show, Eq, Generic, Data)

instance FromJSON JWTResponse where
  parseJSON = genericParseJSON optionsJWTResponse
instance ToJSON JWTResponse where
  toJSON = genericToJSON optionsJWTResponse

optionsJWTResponse :: Options
optionsJWTResponse =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("jWTResponseSuccess", "Success")
      , ("jWTResponseFailure", "Failure")
      ]


-- | login request
data LoginRequest = LoginRequest
  { loginRequestUsernameUnderscoreorUnderscoreemail :: Text -- ^ username or email
  , loginRequestPassword :: Text -- ^ password
  } deriving (Show, Eq, Generic, Data)

instance FromJSON LoginRequest where
  parseJSON = genericParseJSON optionsLoginRequest
instance ToJSON LoginRequest where
  toJSON = genericToJSON optionsLoginRequest

optionsLoginRequest :: Options
optionsLoginRequest =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("loginRequestUsernameUnderscoreorUnderscoreemail", "username_or_email")
      , ("loginRequestPassword", "password")
      ]


-- | problem response
data ProblemResponse = ProblemResponse
  { problemResponseSuccess :: Maybe Text -- ^ problem definition
  , problemResponseFailure :: Maybe Text -- ^ failure message
  } deriving (Show, Eq, Generic, Data)

instance FromJSON ProblemResponse where
  parseJSON = genericParseJSON optionsProblemResponse
instance ToJSON ProblemResponse where
  toJSON = genericToJSON optionsProblemResponse

optionsProblemResponse :: Options
optionsProblemResponse =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("problemResponseSuccess", "Success")
      , ("problemResponseFailure", "Failure")
      ]


-- | number of problems
data ProblemsResponse = ProblemsResponse
  { problemsResponseNumberUnderscoreofUnderscoreproblems :: Double -- ^ number of problems
  } deriving (Show, Eq, Generic, Data)

instance FromJSON ProblemsResponse where
  parseJSON = genericParseJSON optionsProblemsResponse
instance ToJSON ProblemsResponse where
  toJSON = genericToJSON optionsProblemsResponse

optionsProblemsResponse :: Options
optionsProblemsResponse =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("problemsResponseNumberUnderscoreofUnderscoreproblems", "number_of_problems")
      ]


-- | register request
data RegisterRequest = RegisterRequest
  { registerRequestUsername :: Text -- ^ username
  , registerRequestEmail :: Text -- ^ must be a valid email
  , registerRequestPassword :: Text -- ^ password
  } deriving (Show, Eq, Generic, Data)

instance FromJSON RegisterRequest where
  parseJSON = genericParseJSON optionsRegisterRequest
instance ToJSON RegisterRequest where
  toJSON = genericToJSON optionsRegisterRequest

optionsRegisterRequest :: Options
optionsRegisterRequest =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("registerRequestUsername", "username")
      , ("registerRequestEmail", "email")
      , ("registerRequestPassword", "password")
      ]


-- | global score board
data Scoreboard = Scoreboard
  { scoreboardFrozen :: Bool -- ^ Frozen means scoreboard is frozen at the last updated point, so you are seeing the snapshot from that moment.
  , scoreboardScoreboard :: [ScoreboardItem] -- ^ sorted by scores
  , scoreboardUpdatedUnderscoreat :: Text -- ^ date
  } deriving (Show, Eq, Generic, Data)

instance FromJSON Scoreboard where
  parseJSON = genericParseJSON optionsScoreboard
instance ToJSON Scoreboard where
  toJSON = genericToJSON optionsScoreboard

optionsScoreboard :: Options
optionsScoreboard =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("scoreboardFrozen", "frozen")
      , ("scoreboardScoreboard", "scoreboard")
      , ("scoreboardUpdatedUnderscoreat", "updated_at")
      ]


-- | username and score
data ScoreboardItem = ScoreboardItem
  { scoreboardItemUsername :: Text -- ^ user name
  , scoreboardItemScore :: Double -- ^ score
  } deriving (Show, Eq, Generic, Data)

instance FromJSON ScoreboardItem where
  parseJSON = genericParseJSON optionsScoreboardItem
instance ToJSON ScoreboardItem where
  toJSON = genericToJSON optionsScoreboardItem

optionsScoreboardItem :: Options
optionsScoreboardItem =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("scoreboardItemUsername", "username")
      , ("scoreboardItemScore", "score")
      ]


-- | submission
data Submission = Submission
  { submissionSubmission :: SubmissionInfo -- ^ 
  , submissionContents :: Text -- ^ submission contents
  } deriving (Show, Eq, Generic, Data)

instance FromJSON Submission where
  parseJSON = genericParseJSON optionsSubmission
instance ToJSON Submission where
  toJSON = genericToJSON optionsSubmission

optionsSubmission :: Options
optionsSubmission =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("submissionSubmission", "submission")
      , ("submissionContents", "contents")
      ]


-- | submission metadata
data SubmissionInfo = SubmissionInfo
  { submissionInfoUnderscoreid :: Text -- ^ submission id
  , submissionInfoProblemUnderscoreid :: Int -- ^ problem id
  , submissionInfoUserUnderscoreid :: Text -- ^ user id
  , submissionInfoScore :: Value -- ^ submission result from judge
  , submissionInfoSubmittedUnderscoreat :: Text -- ^ submission time as UTC
  } deriving (Show, Eq, Generic, Data)

instance FromJSON SubmissionInfo where
  parseJSON = genericParseJSON optionsSubmissionInfo
instance ToJSON SubmissionInfo where
  toJSON = genericToJSON optionsSubmissionInfo

optionsSubmissionInfo :: Options
optionsSubmissionInfo =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("submissionInfoUnderscoreid", "_id")
      , ("submissionInfoProblemUnderscoreid", "problem_id")
      , ("submissionInfoUserUnderscoreid", "user_id")
      , ("submissionInfoScore", "score")
      , ("submissionInfoSubmittedUnderscoreat", "submitted_at")
      ]


-- | content and problem id
data SubmissionRequest = SubmissionRequest
  { submissionRequestProblemUnderscoreid :: Int -- ^ problem id
  , submissionRequestContents :: Text -- ^ submission contents
  } deriving (Show, Eq, Generic, Data)

instance FromJSON SubmissionRequest where
  parseJSON = genericParseJSON optionsSubmissionRequest
instance ToJSON SubmissionRequest where
  toJSON = genericToJSON optionsSubmissionRequest

optionsSubmissionRequest :: Options
optionsSubmissionRequest =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("submissionRequestProblemUnderscoreid", "problem_id")
      , ("submissionRequestContents", "contents")
      ]


-- | submission
data SubmissionResponse = SubmissionResponse
  { submissionResponseSuccess :: Maybe Submission -- ^ 
  , submissionResponseFailure :: Maybe Text -- ^ failure message
  } deriving (Show, Eq, Generic, Data)

instance FromJSON SubmissionResponse where
  parseJSON = genericParseJSON optionsSubmissionResponse
instance ToJSON SubmissionResponse where
  toJSON = genericToJSON optionsSubmissionResponse

optionsSubmissionResponse :: Options
optionsSubmissionResponse =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("submissionResponseSuccess", "Success")
      , ("submissionResponseFailure", "Failure")
      ]


-- | submissions response
data SubmissionsResponse = SubmissionsResponse
  { submissionsResponseSuccess :: Maybe [Submission] -- ^ submissions
  , submissionsResponseFailure :: Maybe Text -- ^ failure message
  } deriving (Show, Eq, Generic, Data)

instance FromJSON SubmissionsResponse where
  parseJSON = genericParseJSON optionsSubmissionsResponse
instance ToJSON SubmissionsResponse where
  toJSON = genericToJSON optionsSubmissionsResponse

optionsSubmissionsResponse :: Options
optionsSubmissionsResponse =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("submissionsResponseSuccess", "Success")
      , ("submissionsResponseFailure", "Failure")
      ]


-- | userboard response
data UserboardResponse = UserboardResponse
  { userboardResponseSuccess :: Maybe UserboardResponseSuccess -- ^ 
  , userboardResponseFailure :: Maybe Text -- ^ failure message
  } deriving (Show, Eq, Generic, Data)

instance FromJSON UserboardResponse where
  parseJSON = genericParseJSON optionsUserboardResponse
instance ToJSON UserboardResponse where
  toJSON = genericToJSON optionsUserboardResponse

optionsUserboardResponse :: Options
optionsUserboardResponse =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("userboardResponseSuccess", "Success")
      , ("userboardResponseFailure", "Failure")
      ]


-- | 
data UserboardResponseSuccess = UserboardResponseSuccess
  { userboardResponseSuccessProblems :: [Double] -- ^ the highest score for each problem
  } deriving (Show, Eq, Generic, Data)

instance FromJSON UserboardResponseSuccess where
  parseJSON = genericParseJSON optionsUserboardResponseSuccess
instance ToJSON UserboardResponseSuccess where
  toJSON = genericToJSON optionsUserboardResponseSuccess

optionsUserboardResponseSuccess :: Options
optionsUserboardResponseSuccess =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("userboardResponseSuccessProblems", "problems")
      ]

