{-# LANGUAGE OverloadedStrings #-}

module Web.Kamyu.Json
    ( JsonError(..)
    , jsonBody
    , jsonResponse
    , jsonResponseWith
    , json
    , jsonWith
    , jsonWithStatus
    , jsonWithStatusRequest
    ) where

import Data.Aeson (FromJSON, ToJSON, eitherDecode, encode)
import Network.HTTP.Types (Status, hContentType, status200)
import Network.Wai (Request, Response, responseLBS, strictRequestBody)
import Web.Kamyu.Core (KamyuHandler)
import Web.Kamyu.Status (badRequest)

-- | Possible errors when we parse JSON request bodies
newtype JsonError = JsonDecodeError String deriving (Show, Eq)

-- | Parse request body as JSON using the provided target type
jsonBody :: FromJSON body => Request -> IO (Either JsonError body)
jsonBody req = do
    body <- strictRequestBody req
    case eitherDecode body of
        Left err -> pure (Left (JsonDecodeError err))
        Right val -> pure (Right val)

-- | Create a JSON response with `application/json` content type (defaults to 200)
jsonResponse :: ToJSON a => a -> Response
jsonResponse = jsonResponseWith status200

-- | Same as 'jsonResponse' but lets you pick a custom Status
jsonResponseWith :: ToJSON a => Status -> a -> Response
jsonResponseWith statusCode value =
    responseLBS statusCode [(hContentType, "application/json")] (encode value)

-- | Build a Kamyu handler from a pure JSON handler, similar to Spring controllers.
-- Uses HTTP 200 by default. For custom statuses use 'jsonWithStatus'.
json :: (FromJSON body, ToJSON result) => (body -> IO result) -> KamyuHandler
json handler = jsonWithStatus (\body -> do
    output <- handler body
    pure (status200, output))

-- | Same as 'json' but also gives access to the raw Request.
-- Uses HTTP 200 by default. For custom statuses use 'jsonWithStatusRequest'.
jsonWith :: (FromJSON body, ToJSON result) => (body -> Request -> IO result) -> KamyuHandler
jsonWith handler = jsonWithStatusRequest (\body req -> do
    output <- handler body req
    pure (status200, output))

-- | JSON handler where the user decides which HTTP status to return.
jsonWithStatus :: (FromJSON body, ToJSON result) => (body -> IO (Status, result)) -> KamyuHandler
jsonWithStatus handler = jsonWithStatusRequest (\body _ -> handler body)

-- | Most flexible variant – gives access to Request and lets you choose a status.
jsonWithStatusRequest :: (FromJSON body, ToJSON result)
                      => (body -> Request -> IO (Status, result))
                      -> KamyuHandler
jsonWithStatusRequest handler request params = do  -- ← Добавили params
    parsed <- jsonBody request
    case parsed of
        Left err -> pure $ badRequest (jsonErrorMessage err)
        Right body -> do
            (statusCode, output) <- handler body request
            pure (jsonResponseWith statusCode output)

jsonErrorMessage :: JsonError -> String
jsonErrorMessage (JsonDecodeError msg) = "Failed to decode JSON: " ++ msg