# Kamyu

**Be simple, write less**

Kamyu is a small Haskell web framework built on top of WAI and Warp. It focuses
on a compact DSL for routes, request parameters, middleware, and JSON handlers.

The project is still experimental, but the goal is simple: make small Haskell
web apps feel approachable.

## Quick Start

```haskell
module Main where

import Web.Kamyu

main :: IO ()
main =
  runKamyu 8080 $ do
    get "/" $ \_ _ -> do
      pure $ ok "SUCCESS! Kamyu is working!"
```

Run the example app:

```shell
stack run
```

## Routing

```haskell
get "/" homeHandler
post "/users" createUserHandler
put "/users/:id" updateUserHandler
delete "/users/:id" deleteUserHandler
patch "/users/:id" patchUserHandler
```

Handlers receive the original WAI `Request` and the matched path parameters:

```haskell
homeHandler :: KamyuHandler
homeHandler _ _ =
  pure $ ok "Home is here"
```

## Path Parameters

Dynamic path segments start with `:`.

```haskell
get "/users/:id" $ \_ params -> do
  let userId = fromPath "id" params `orDefault` "0"
  pure $ ok $ "User ID: " ++ userId
```

Typed helpers are available for path parameters:

```haskell
get "/users/:id" $ \_ params -> do
  let userId = fromPathInt "id" params `orDefault` 0
  pure $ ok $ "User ID: " ++ show userId
```

## Query Parameters

```haskell
get "/search" $ \req _ -> do
  let query = fromQuery "q" req `orElse` ""
      page = fromQueryInt "page" req `orElse` 1
  pure $ ok $ "Search: " ++ query ++ ", page: " ++ show page
```

Available query helpers:

```haskell
fromQuery       :: String -> Request -> Maybe String
fromQueryInt    :: String -> Request -> Maybe Int
fromQueryBool   :: String -> Request -> Maybe Bool
fromQueryDouble :: String -> Request -> Maybe Double
```

## Nested Routes

Use `path` to add a route prefix, `capture` to add a dynamic segment, and `root`
to temporarily return to the root context.

```haskell
path "api" $ do
  get "/status" $ \_ _ ->
    pure $ ok "API is alive"

  path "users" $
    capture "id" $
      get "/profile" $ \_ params -> do
        let userId = fromPath "id" params `orDefault` "unknown"
        pure $ ok $ "Profile: " ++ userId

  root $
    get "/health" $ \_ _ ->
      pure $ ok "Server health"
```

This registers:

```text
GET /api/status
GET /api/users/:id/profile
GET /health
```

## JSON Helpers

For simple JSON endpoints, define request/response types and a small function.

```haskell
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

import GHC.Generics (Generic)
import Web.Kamyu

data CreateUser = CreateUser
  { createUserName :: String
  }
  deriving (Show, Generic, JsonCodec)

data User = User
  { userId :: Int,
    userName :: String
  }
  deriving (Show, Generic, JsonCodec)

createUser :: CreateUser -> IO User
createUser CreateUser {createUserName = name} =
  pure $ User 1 name

main :: IO ()
main =
  runKamyu 8080 $ do
    post "/users" $ jsonCreate createUser
```

`jsonCreate` parses the request body as JSON, calls your function, serializes the
result, sets `Content-Type: application/json`, and responds with `201 Created`.

There are also variants for other cases:

```haskell
jsonOk         :: (body -> IO result) -> KamyuHandler
jsonCreate     :: (body -> IO result) -> KamyuHandler
jsonWithStatus :: Status -> (body -> IO result) -> KamyuHandler
```

For handlers that need query parameters, headers, or path parameters, use
`jsonHandler`.

```haskell
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

import GHC.Generics (Generic)
import Network.HTTP.Types (Status, status201)
import Network.Wai (Request)
import Web.Kamyu

data CreatePerson = CreatePerson
  { name :: String,
    age :: Int
  }
  deriving (Show, Generic, JsonCodec)

data Person = Person
  { identifier :: Int,
    fullName :: String,
    personAge :: Int
  }
  deriving (Show, Generic, JsonCodec)

createPerson :: CreatePerson -> Request -> [(String, String)] -> IO (Status, Person)
createPerson CreatePerson {name = personName, age = personAgeVal} req pathParams = do
  let city = fromPath "city" pathParams `orDefault` "unknown"
      source = fromQuery "source" req `orElse` "api"
      decoratedName = personName ++ " from " ++ city ++ " via " ++ source
  pure (status201, Person 1 decoratedName personAgeVal)

main :: IO ()
main =
  runKamyu 8080 $ do
    post "/cities/:city/people" $ jsonHandler createPerson
```

## Middleware

Middleware is standard WAI middleware:

```haskell
type Middleware = Application -> Application
```

Use `createMiddleware` to add middleware to the app.

```haskell
import Web.Kamyu

main :: IO ()
main =
  runKamyu 8080 $ do
    createMiddleware logger
    createMiddleware cors
    createMiddleware $ poweredBy "Kamyu"

    get "/" $ \_ _ ->
      pure $ ok "Hello"
```

Middleware can also be built with conditions:

```haskell
main :: IO ()
main =
  runKamyu 8080 $ do
    createMiddleware $
      buildMiddleware $
        use (auth (== "super-secret"))
          |> exceptRoute "/"
          |> exceptRoute "/health"

    get "/" $ \_ _ ->
      pure $ ok "Public"

    get "/private" $ \_ _ ->
      pure $ ok "Private"

    get "/health" $ \_ _ ->
      pure $ ok "OK"
```

Built-in middleware helpers:

```haskell
logger    :: Middleware
cors      :: Middleware
poweredBy :: String -> Middleware
auth      :: (ByteString -> Bool) -> Middleware
```

## Module Layout

```text
Web.Kamyu.Core         core types
Web.Kamyu.Router       route registration and matching
Web.Kamyu.Combinators  route DSL
Web.Kamyu.Server       WAI/Warp application runner
Web.Kamyu.Params       path and query parameter helpers
Web.Kamyu.Json         JSON request/response helpers
Web.Kamyu.Middleware   middleware builder and built-ins
Web.Kamyu              public umbrella import
```

## Roadmap

- Done: GET, POST, PUT, DELETE, PATCH routes
- Done: path and query parameters
- Done: nested route combinators
- Done: middleware support
- Done: JSON helpers
- Next: route conflict detection
- Next: static files
- Next: better error responses

> Kamyu is experimental and changing quickly.
