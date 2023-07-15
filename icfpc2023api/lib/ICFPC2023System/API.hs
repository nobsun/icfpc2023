{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE ViewPatterns               #-}
{-# OPTIONS_GHC
-fno-warn-unused-binds -fno-warn-unused-imports -freduction-depth=328 #-}

module ICFPC2023System.API
  ( -- * Client and Server
    Config(..)
  , ICFPC2023SystemBackend(..)
  , createICFPC2023SystemClient
  , runICFPC2023SystemServer
  , runICFPC2023SystemMiddlewareServer
  , runICFPC2023SystemClient
  , runICFPC2023SystemClientWithManager
  , callICFPC2023System
  , ICFPC2023SystemClient
  , ICFPC2023SystemClientError(..)
  -- ** Servant
  , ICFPC2023SystemAPI
  -- ** Plain WAI Application
  , serverWaiApplicationICFPC2023System
  -- ** Authentication
  , ICFPC2023SystemAuth(..)
  , clientAuth
  , Protected
  ) where

import           ICFPC2023System.Types

import           Control.Monad.Catch                (Exception, MonadThrow, throwM)
import           Control.Monad.Except               (ExceptT, runExceptT)
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Reader         (ReaderT (..))
import           Data.Aeson                         (Value)
import           Data.ByteString                    (ByteString)
import           Data.Coerce                        (coerce)
import           Data.Data                          (Data)
import           Data.Function                      ((&))
import qualified Data.Map                           as Map
import           Data.Monoid                        ((<>))
import           Data.Proxy                         (Proxy (..))
import           Data.Set                           (Set)
import           Data.Text                          (Text)
import qualified Data.Text                          as T
import           Data.Time
import           Data.UUID                          (UUID)
import           GHC.Exts                           (IsString (..))
import           GHC.Generics                       (Generic)
import           Network.HTTP.Client                (Manager, newManager)
import           Network.HTTP.Client.TLS            (tlsManagerSettings)
import           Network.HTTP.Types.Method          (methodOptions)
import           Network.Wai                        (Middleware, Request, requestHeaders)
import qualified Network.Wai.Handler.Warp           as Warp
import           Network.Wai.Middleware.HttpAuth    (extractBearerAuth)
import           Servant                            (ServerError, serveWithContextT, throwError)
import           Servant.API                        hiding (addHeader)
import           Servant.API.Verbs                  (StdMethod (..), Verb)
import           Servant.API.Experimental.Auth      (AuthProtect)
import           Servant.Client                     (ClientEnv, Scheme (Http), ClientError, client,
                                                     mkClientEnv, parseBaseUrl)
import           Servant.Client.Core                (baseUrlPort, baseUrlHost, AuthClientData, AuthenticatedRequest, addHeader, mkAuthenticatedRequest)
import           Servant.Client.Internal.HttpClient (ClientM (..))
import           Servant.Server                     (Handler (..), Application, Context ((:.), EmptyContext))
import           Servant.Server.Experimental.Auth   (AuthHandler, AuthServerData, mkAuthHandler)
import           Servant.Server.StaticFiles         (serveDirectoryFileServer)
import           Web.FormUrlEncoded
import           Web.HttpApiData




-- | List of elements parsed from a query.
newtype QueryList (p :: CollectionFormat) a = QueryList
  { fromQueryList :: [a]
  } deriving (Functor, Applicative, Monad, Foldable, Traversable)

-- | Formats in which a list can be encoded into a HTTP path.
data CollectionFormat
  = CommaSeparated -- ^ CSV format for multiple parameters.
  | SpaceSeparated -- ^ Also called "SSV"
  | TabSeparated -- ^ Also called "TSV"
  | PipeSeparated -- ^ `value1|value2|value2`
  | MultiParamArray -- ^ Using multiple GET parameters, e.g. `foo=bar&foo=baz`. Only for GET params.

instance FromHttpApiData a => FromHttpApiData (QueryList 'CommaSeparated a) where
  parseQueryParam = parseSeparatedQueryList ','

instance FromHttpApiData a => FromHttpApiData (QueryList 'TabSeparated a) where
  parseQueryParam = parseSeparatedQueryList '\t'

instance FromHttpApiData a => FromHttpApiData (QueryList 'SpaceSeparated a) where
  parseQueryParam = parseSeparatedQueryList ' '

instance FromHttpApiData a => FromHttpApiData (QueryList 'PipeSeparated a) where
  parseQueryParam = parseSeparatedQueryList '|'

instance FromHttpApiData a => FromHttpApiData (QueryList 'MultiParamArray a) where
  parseQueryParam = error "unimplemented FromHttpApiData for MultiParamArray collection format"

parseSeparatedQueryList :: FromHttpApiData a => Char -> Text -> Either Text (QueryList p a)
parseSeparatedQueryList char = fmap QueryList . mapM parseQueryParam . T.split (== char)

instance ToHttpApiData a => ToHttpApiData (QueryList 'CommaSeparated a) where
  toQueryParam = formatSeparatedQueryList ','

instance ToHttpApiData a => ToHttpApiData (QueryList 'TabSeparated a) where
  toQueryParam = formatSeparatedQueryList '\t'

instance ToHttpApiData a => ToHttpApiData (QueryList 'SpaceSeparated a) where
  toQueryParam = formatSeparatedQueryList ' '

instance ToHttpApiData a => ToHttpApiData (QueryList 'PipeSeparated a) where
  toQueryParam = formatSeparatedQueryList '|'

instance ToHttpApiData a => ToHttpApiData (QueryList 'MultiParamArray a) where
  toQueryParam = error "unimplemented ToHttpApiData for MultiParamArray collection format"

formatSeparatedQueryList :: ToHttpApiData a => Char ->  QueryList p a -> Text
formatSeparatedQueryList char = T.intercalate (T.singleton char) . map toQueryParam . fromQueryList


-- | Servant type-level API, generated from the OpenAPI spec for ICFPC2023System.
type ICFPC2023SystemAPI
    =    "login" :> ReqBody '[JSON] LoginRequest :> Verb 'POST 200 '[JSON] JWTResponse -- 'login' route
    :<|> "register" :> ReqBody '[JSON] RegisterRequest :> Verb 'POST 200 '[JSON] JWTResponse -- 'register' route
    :<|> "problems" :> Verb 'GET 200 '[JSON] ProblemsResponse -- 'getNumberOfProblems' route
    :<|> "problem" :> QueryParam "problem_id" Int :> Verb 'GET 200 '[JSON] ProblemResponse -- 'getProblem' route
    :<|> "scoreboard" :> Verb 'GET 200 '[JSON] Scoreboard -- 'getScoreboard' route
    :<|> Protected :> "userboard" :> Verb 'GET 200 '[JSON] UserboardResponse -- 'getUserboard' route
    :<|> Protected :> "submission" :> QueryParam "submission-id" Text :> Verb 'GET 200 '[JSON] SubmissionResponse -- 'getSubmission' route
    :<|> Protected :> "submissions" :> QueryParam "offset" Int :> QueryParam "limit" Int :> QueryParam "problem_id" Int :> Verb 'GET 200 '[JSON] SubmissionsResponse -- 'getSubmissions' route
    :<|> Protected :> "submission" :> ReqBody '[JSON] SubmissionRequest :> Verb 'POST 200 '[JSON] Text -- 'postSubmission' route
    :<|> Raw


-- | Server or client configuration, specifying the host and port to query or serve on.
data Config = Config
  { configUrl :: String  -- ^ scheme://hostname:port/path, e.g. "http://localhost:8080/"
  } deriving (Eq, Ord, Show, Read)


-- | Custom exception type for our errors.
newtype ICFPC2023SystemClientError = ICFPC2023SystemClientError ClientError
  deriving (Show, Exception)
-- | Configuration, specifying the full url of the service.


-- | Backend for ICFPC2023System.
-- The backend can be used both for the client and the server. The client generated from the ICFPC2023System OpenAPI spec
-- is a backend that executes actions by sending HTTP requests (see @createICFPC2023SystemClient@). Alternatively, provided
-- a backend, the API can be served using @runICFPC2023SystemMiddlewareServer@.
data ICFPC2023SystemBackend a m = ICFPC2023SystemBackend
  { login :: LoginRequest -> m JWTResponse{- ^ Login with your account -}
  , register :: RegisterRequest -> m JWTResponse{- ^ Register your account to the contest. -}
  , getNumberOfProblems :: m ProblemsResponse{- ^ Get number of problems. -}
  , getProblem :: Maybe Int -> m ProblemResponse{- ^ Get problems contents with problem id. -}
  , getScoreboard :: m Scoreboard{- ^ Get the global scoreboard. -}
  , getUserboard :: a -> m UserboardResponse{- ^ Get scoreboard for your account. Provides the highest score for each problem. -}
  , getSubmission :: a -> Maybe Text -> m SubmissionResponse{- ^ Get submission with ID. You can only get submissions linked to your account. -}
  , getSubmissions :: a -> Maybe Int -> Maybe Int -> Maybe Int -> m SubmissionsResponse{- ^ Get [limit] number of your past submissions starting from a given [offset], sorted by submission time. -}
  , postSubmission :: a -> SubmissionRequest -> m Text{- ^ Post submission with contents and problem id. -}
  }

-- | Authentication settings for ICFPC2023System.
-- lookupUser is used to retrieve a user given a header value. The data type can be specified by providing an
-- type instance for AuthServerData. authError is a function that given a request returns a custom error that
-- is returned when the header is not found.
data ICFPC2023SystemAuth = ICFPC2023SystemAuth
  { lookupUser :: ByteString -> Handler AuthServer
  , authError :: Request -> ServerError
  }

newtype ICFPC2023SystemClient a = ICFPC2023SystemClient
  { runClient :: ClientEnv -> ExceptT ClientError IO a
  } deriving Functor

instance Applicative ICFPC2023SystemClient where
  pure x = ICFPC2023SystemClient (\_ -> pure x)
  (ICFPC2023SystemClient f) <*> (ICFPC2023SystemClient x) =
    ICFPC2023SystemClient (\env -> f env <*> x env)

instance Monad ICFPC2023SystemClient where
  (ICFPC2023SystemClient a) >>= f =
    ICFPC2023SystemClient (\env -> do
      value <- a env
      runClient (f value) env)

instance MonadIO ICFPC2023SystemClient where
  liftIO io = ICFPC2023SystemClient (\_ -> liftIO io)

createICFPC2023SystemClient :: ICFPC2023SystemBackend AuthClient ICFPC2023SystemClient
createICFPC2023SystemClient = ICFPC2023SystemBackend{..}
  where
    ((coerce -> login) :<|>
     (coerce -> register) :<|>
     (coerce -> getNumberOfProblems) :<|>
     (coerce -> getProblem) :<|>
     (coerce -> getScoreboard) :<|>
     (coerce -> getUserboard) :<|>
     (coerce -> getSubmission) :<|>
     (coerce -> getSubmissions) :<|>
     (coerce -> postSubmission) :<|>
     _) = client (Proxy :: Proxy ICFPC2023SystemAPI)

-- | Run requests in the ICFPC2023SystemClient monad.
runICFPC2023SystemClient :: Config -> ICFPC2023SystemClient a -> ExceptT ClientError IO a
runICFPC2023SystemClient clientConfig cl = do
  manager <- liftIO $ newManager tlsManagerSettings
  runICFPC2023SystemClientWithManager manager clientConfig cl

-- | Run requests in the ICFPC2023SystemClient monad using a custom manager.
runICFPC2023SystemClientWithManager :: Manager -> Config -> ICFPC2023SystemClient a -> ExceptT ClientError IO a
runICFPC2023SystemClientWithManager manager Config{..} cl = do
  url <- parseBaseUrl configUrl
  runClient cl $ mkClientEnv manager url

-- | Like @runClient@, but returns the response or throws
--   a ICFPC2023SystemClientError
callICFPC2023System
  :: (MonadIO m, MonadThrow m)
  => ClientEnv -> ICFPC2023SystemClient a -> m a
callICFPC2023System env f = do
  res <- liftIO $ runExceptT $ runClient f env
  case res of
    Left err       -> throwM (ICFPC2023SystemClientError err)
    Right response -> pure response


requestMiddlewareId :: Application -> Application
requestMiddlewareId a = a

-- | Run the ICFPC2023System server at the provided host and port.
runICFPC2023SystemServer
  :: (MonadIO m, MonadThrow m)
  => Config -> ICFPC2023SystemAuth -> ICFPC2023SystemBackend AuthServer (ExceptT ServerError IO) -> m ()
runICFPC2023SystemServer config auth backend = runICFPC2023SystemMiddlewareServer config requestMiddlewareId auth backend

-- | Run the ICFPC2023System server at the provided host and port.
runICFPC2023SystemMiddlewareServer
  :: (MonadIO m, MonadThrow m)
  => Config -> Middleware -> ICFPC2023SystemAuth -> ICFPC2023SystemBackend AuthServer (ExceptT ServerError IO) -> m ()
runICFPC2023SystemMiddlewareServer Config{..} middleware auth backend = do
  url <- parseBaseUrl configUrl
  let warpSettings = Warp.defaultSettings
        & Warp.setPort (baseUrlPort url)
        & Warp.setHost (fromString $ baseUrlHost url)
  liftIO $ Warp.runSettings warpSettings $ middleware $ serverWaiApplicationICFPC2023System auth backend

-- | Plain "Network.Wai" Application for the ICFPC2023System server.
--
-- Can be used to implement e.g. tests that call the API without a full webserver.
serverWaiApplicationICFPC2023System :: ICFPC2023SystemAuth -> ICFPC2023SystemBackend AuthServer (ExceptT ServerError IO) -> Application
serverWaiApplicationICFPC2023System auth backend = serveWithContextT (Proxy :: Proxy ICFPC2023SystemAPI) context id (serverFromBackend backend)
  where
    context = serverContext auth
    serverFromBackend ICFPC2023SystemBackend{..} =
      (coerce login :<|>
       coerce register :<|>
       coerce getNumberOfProblems :<|>
       coerce getProblem :<|>
       coerce getScoreboard :<|>
       coerce getUserboard :<|>
       coerce getSubmission :<|>
       coerce getSubmissions :<|>
       coerce postSubmission :<|>
       serveDirectoryFileServer "static")

-- Authentication is implemented with servants generalized authentication:
-- https://docs.servant.dev/en/stable/tutorial/Authentication.html#generalized-authentication

authHandler :: ICFPC2023SystemAuth -> AuthHandler Request AuthServer
authHandler ICFPC2023SystemAuth{..} = mkAuthHandler handler
  where
    handler req = case lookup "Authorization" (requestHeaders req) of
      Just header -> case extractBearerAuth header of
        Just key -> lookupUser key
        Nothing -> throwError (authError req)
      Nothing -> throwError (authError req)

type Protected = AuthProtect "bearer"
type AuthServer = AuthServerData Protected
type AuthClient = AuthenticatedRequest Protected
type instance AuthClientData Protected = Text

clientAuth :: Text -> AuthClient
clientAuth key = mkAuthenticatedRequest ("Bearer " <> key) (addHeader "Authorization")

serverContext :: ICFPC2023SystemAuth -> Context (AuthHandler Request AuthServer ': '[])
serverContext auth = authHandler auth :. EmptyContext
