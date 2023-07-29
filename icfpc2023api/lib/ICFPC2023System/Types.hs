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
import Data.List (stripPrefix)
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
import Data.Function ((&))


-- | your JWT access token
data JWTResponse = JWTResponse
  { jWTResponseSuccess :: Maybe Text -- ^ your JWT access token
  , jWTResponseFailure :: Maybe Text -- ^ failure message
  } deriving (Show, Eq, Generic, Data)

instance FromJSON JWTResponse where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "jWTResponse")
instance ToJSON JWTResponse where
  toJSON = genericToJSON (removeFieldLabelPrefix False "jWTResponse")


-- | login request
data LoginRequest = LoginRequest
  { loginRequestUsernameUnderscoreorUnderscoreemail :: Text -- ^ username or email
  , loginRequestPassword :: Text -- ^ password
  } deriving (Show, Eq, Generic, Data)

instance FromJSON LoginRequest where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "loginRequest")
instance ToJSON LoginRequest where
  toJSON = genericToJSON (removeFieldLabelPrefix False "loginRequest")


-- | problem response
data ProblemResponse = ProblemResponse
  { problemResponseSuccess :: Maybe Text -- ^ problem definition
  , problemResponseFailure :: Maybe Text -- ^ failure message
  } deriving (Show, Eq, Generic, Data)

instance FromJSON ProblemResponse where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "problemResponse")
instance ToJSON ProblemResponse where
  toJSON = genericToJSON (removeFieldLabelPrefix False "problemResponse")


-- | number of problems
data ProblemsResponse = ProblemsResponse
  { problemsResponseNumberUnderscoreofUnderscoreproblems :: Double -- ^ number of problems
  } deriving (Show, Eq, Generic, Data)

instance FromJSON ProblemsResponse where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "problemsResponse")
instance ToJSON ProblemsResponse where
  toJSON = genericToJSON (removeFieldLabelPrefix False "problemsResponse")


-- | register request
data RegisterRequest = RegisterRequest
  { registerRequestUsername :: Text -- ^ username
  , registerRequestEmail :: Text -- ^ must be a valid email
  , registerRequestPassword :: Text -- ^ password
  } deriving (Show, Eq, Generic, Data)

instance FromJSON RegisterRequest where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "registerRequest")
instance ToJSON RegisterRequest where
  toJSON = genericToJSON (removeFieldLabelPrefix False "registerRequest")


-- | global score board
data Scoreboard = Scoreboard
  { scoreboardFrozen :: Bool -- ^ Frozen means scoreboard is frozen at the last updated point, so you are seeing the snapshot from that moment.
  , scoreboardScoreboard :: [ScoreboardItem] -- ^ sorted by scores
  , scoreboardUpdatedUnderscoreat :: Text -- ^ date
  } deriving (Show, Eq, Generic, Data)

instance FromJSON Scoreboard where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "scoreboard")
instance ToJSON Scoreboard where
  toJSON = genericToJSON (removeFieldLabelPrefix False "scoreboard")


-- | username and score
data ScoreboardItem = ScoreboardItem
  { scoreboardItemUsername :: Text -- ^ user name
  , scoreboardItemScore :: Double -- ^ score
  } deriving (Show, Eq, Generic, Data)

instance FromJSON ScoreboardItem where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "scoreboardItem")
instance ToJSON ScoreboardItem where
  toJSON = genericToJSON (removeFieldLabelPrefix False "scoreboardItem")


-- | submission
data Submission = Submission
  { submissionSubmission :: SubmissionInfo -- ^ 
  , submissionContents :: Text -- ^ submission contents
  } deriving (Show, Eq, Generic, Data)

instance FromJSON Submission where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "submission")
instance ToJSON Submission where
  toJSON = genericToJSON (removeFieldLabelPrefix False "submission")


-- | submission metadata
data SubmissionInfo = SubmissionInfo
  { submissionInfoUnderscoreid :: Text -- ^ submission id
  , submissionInfoProblemUnderscoreid :: Int -- ^ problem id
  , submissionInfoUserUnderscoreid :: Text -- ^ user id
  , submissionInfoScore :: Value -- ^ submission result from judge
  , submissionInfoSubmittedUnderscoreat :: Text -- ^ submission time as UTC
  } deriving (Show, Eq, Generic, Data)

instance FromJSON SubmissionInfo where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "submissionInfo")
instance ToJSON SubmissionInfo where
  toJSON = genericToJSON (removeFieldLabelPrefix False "submissionInfo")


-- | content and problem id
data SubmissionRequest = SubmissionRequest
  { submissionRequestProblemUnderscoreid :: Int -- ^ problem id
  , submissionRequestContents :: Text -- ^ submission contents
  } deriving (Show, Eq, Generic, Data)

instance FromJSON SubmissionRequest where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "submissionRequest")
instance ToJSON SubmissionRequest where
  toJSON = genericToJSON (removeFieldLabelPrefix False "submissionRequest")


-- | submission
data SubmissionResponse = SubmissionResponse
  { submissionResponseSuccess :: Maybe Submission -- ^ 
  , submissionResponseFailure :: Maybe Text -- ^ failure message
  } deriving (Show, Eq, Generic, Data)

instance FromJSON SubmissionResponse where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "submissionResponse")
instance ToJSON SubmissionResponse where
  toJSON = genericToJSON (removeFieldLabelPrefix False "submissionResponse")


-- | submissions response
data SubmissionsResponse = SubmissionsResponse
  { submissionsResponseSuccess :: Maybe [Submission] -- ^ submissions
  , submissionsResponseFailure :: Maybe Text -- ^ failure message
  } deriving (Show, Eq, Generic, Data)

instance FromJSON SubmissionsResponse where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "submissionsResponse")
instance ToJSON SubmissionsResponse where
  toJSON = genericToJSON (removeFieldLabelPrefix False "submissionsResponse")


-- | userboard response
data UserboardResponse = UserboardResponse
  { userboardResponseSuccess :: Maybe UserboardResponseSuccess -- ^ 
  , userboardResponseFailure :: Maybe Text -- ^ failure message
  } deriving (Show, Eq, Generic, Data)

instance FromJSON UserboardResponse where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "userboardResponse")
instance ToJSON UserboardResponse where
  toJSON = genericToJSON (removeFieldLabelPrefix False "userboardResponse")


-- | 
data UserboardResponseSuccess = UserboardResponseSuccess
  { userboardResponseSuccessProblems :: [Double] -- ^ the highest score for each problem
  } deriving (Show, Eq, Generic, Data)

instance FromJSON UserboardResponseSuccess where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "userboardResponseSuccess")
instance ToJSON UserboardResponseSuccess where
  toJSON = genericToJSON (removeFieldLabelPrefix False "userboardResponseSuccess")


uncapitalize :: String -> String
uncapitalize (first:rest) = Char.toLower first : rest
uncapitalize [] = []

-- | Remove a field label prefix during JSON parsing.
--   Also perform any replacements for special characters.
--   The @forParsing@ parameter is to distinguish between the cases in which we're using this
--   to power a @FromJSON@ or a @ToJSON@ instance. In the first case we're parsing, and we want
--   to replace special characters with their quoted equivalents (because we cannot have special
--   chars in identifier names), while we want to do vice versa when sending data instead.
removeFieldLabelPrefix :: Bool -> String -> Options
removeFieldLabelPrefix forParsing prefix =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = uncapitalize . fromMaybe (error ("did not find prefix " ++ prefix)) . stripPrefix prefix . replaceSpecialChars
    }
  where
    replaceSpecialChars field = foldl (&) field (map mkCharReplacement specialChars)
    specialChars =
      [ ("$", "'Dollar")
      , ("^", "'Caret")
      , ("|", "'Pipe")
      , ("=", "'Equal")
      , ("*", "'Star")
      , ("-", "'Dash")
      , ("&", "'Ampersand")
      , ("%", "'Percent")
      , ("#", "'Hash")
      , ("@", "'At")
      , ("!", "'Exclamation")
      , ("+", "'Plus")
      , (":", "'Colon")
      , (";", "'Semicolon")
      , (">", "'GreaterThan")
      , ("<", "'LessThan")
      , (".", "'Period")
      , ("_", "'Underscore")
      , ("?", "'Question_Mark")
      , (",", "'Comma")
      , ("'", "'Quote")
      , ("/", "'Slash")
      , ("(", "'Left_Parenthesis")
      , (")", "'Right_Parenthesis")
      , ("{", "'Left_Curly_Bracket")
      , ("}", "'Right_Curly_Bracket")
      , ("[", "'Left_Square_Bracket")
      , ("]", "'Right_Square_Bracket")
      , ("~", "'Tilde")
      , ("`", "'Backtick")
      , ("<=", "'Less_Than_Or_Equal_To")
      , (">=", "'Greater_Than_Or_Equal_To")
      , ("!=", "'Not_Equal")
      , ("<>", "'Not_Equal")
      , ("~=", "'Tilde_Equal")
      , ("\\", "'Back_Slash")
      , ("\"", "'Double_Quote")
      ]
    mkCharReplacement (replaceStr, searchStr) = T.unpack . replacer (T.pack searchStr) (T.pack replaceStr) . T.pack
    replacer =
      if forParsing
        then flip T.replace
        else T.replace
