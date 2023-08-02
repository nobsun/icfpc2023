# Auto-Generated OpenAPI Bindings to `ICFPC2023System`

The library in `lib` provides auto-generated-from-OpenAPI bindings to the ICFPC2023System API.

## Installation

Installation follows the standard approach to installing Stack-based projects.

1. Install the [Haskell `stack` tool](http://docs.haskellstack.org/en/stable/README).
2. Run `stack install` to install this package.

Otherwise, if you already have a Stack project, you can include this package under the `packages` key in your `stack.yaml`:
```yaml
packages:
- location:
    git: https://github.com/yourGitOrg/yourGitRepo
    commit: somecommit
```

## Main Interface

The main interface to this library is in the `ICFPC2023System.API` module, which exports the ICFPC2023SystemBackend type. The ICFPC2023SystemBackend
type can be used to create and define servers and clients for the API.

## Creating a Client

A client can be created via the `createICFPC2023SystemClient` function, which will generate a function for every endpoint of the API.
Then these functions can be invoked with `runICFPC2023SystemClientWithManager` or more conveniently with `callICFPC2023SystemClient`
(depending if you want an `Either` back or you want to catch) to access the API endpoint they refer to, if the API is served
at the `url` you specified.

For example, if `localhost:8080` is serving the ICFPC2023System API, you can write:

```haskell
{-# LANGUAGE RecordWildCards #-}

import ICFPC2023System.API as API

import           Network.HTTP.Client     (newManager)
import           Network.HTTP.Client.TLS (tlsManagerSettings)
import           Servant.Client          (ClientEnv, mkClientEnv, parseBaseUrl)


main :: IO ()
main = do
  -- Configure the BaseUrl for the client
  url <- parseBaseUrl "http://localhost:8080/"

  -- You probably want to reuse the Manager across calls, for performance reasons
  manager <- newManager tlsManagerSettings

  -- Create the client (all endpoint functions will be available)
  let ICFPC2023SystemBackend{..} = API.createICFPC2023SystemClient

  -- Any ICFPC2023System API call can go here, e.g. here we call `getSomeEndpoint`
  API.callICFPC2023System (mkClientEnv manager url) getSomeEndpoint
```

## Creating a Server

In order to create a server, you must use the `runICFPC2023SystemMiddlewareServer` function. However, you unlike the client, in which case you *got* a `ICFPC2023SystemBackend`
from the library, you must instead *provide* a `ICFPC2023SystemBackend`. For example, if you have defined handler functions for all the
functions in `ICFPC2023System.Handlers`, you can write:

```haskell
{-# LANGUAGE RecordWildCards #-}

import ICFPC2023System.API
-- required dependency: wai
import Network.Wai (Middleware)
-- required dependency: wai-extra
import Network.Wai.Middleware.RequestLogger (logStdout)

-- A module you wrote yourself, containing all handlers needed for the ICFPC2023SystemBackend type.
import ICFPC2023System.Handlers

-- If you would like to not use any middlewares you could use runICFPC2023SystemServer instead

-- Combined middlewares
requestMiddlewares :: Middleware
requestMiddlewares = logStdout

-- Run a ICFPC2023System server on localhost:8080
main :: IO ()
main = do
  let server = ICFPC2023SystemBackend{..}
      config = Config "http://localhost:8080/"
  runICFPC2023SystemMiddlewareServer config requestMiddlewares server
```

## Authentication

Currently basic, bearer and API key authentication is supported. The API key must be provided
in the request header.

For clients authentication the function `clientAuth` is generated automatically. For basic
authentication the argument is of type `BasicAuthData` provided by `Servant.API.BasicAuth`.
For bearer and API key authentication the argument is the key/token and is of type `Text`.
Protected endpoints on the client will receive an extra argument. The value returned by
`clientAuth keyTokenOrBasic` can then be used to make authenticated requests.

For the server you are free to choose a custom data type. After you specified an instance of
`AuthServerData` it is automatically added as a first argument to protected endpoints:

```
newtype Account = Account {unAccount :: Text}
type instance AuthServerData Protected = Account
```

Additionally, you have to provide value for the `ICFPC2023SystemAuth` type provided by the
`ICFPC2023System.API` module:

```
auth :: ICFPC2023SystemAuth
auth =
  ICFPC2023SystemAuth
    { lookupUser = lookupAccount,
      authError = \request -> err401 {errBody = "Missing header"}
    }
```

`lookupAccount` is a user defined function used to verify the key, token or basic auth data.
`authError` takes a `Request` and returns a `ServerError`. The value is used by the server
functions:

```
runICFPC2023SystemMiddlewareServer config requestMiddlewares auth server
```
