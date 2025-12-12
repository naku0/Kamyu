{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Web.Kamyu.Json
  ( JsonError (..),
    jsonBody,
    jsonResponse,
    jsonResponseWith,
    jsonHandler,
    JsonCodec (..),
  )
where

import Data.Aeson
  ( FromJSON (..),
    ToJSON (..),
    Value,
    defaultOptions,
    eitherDecode,
    encode,
    genericParseJSON,
    genericToJSON,
  )
import Data.Aeson.Types (GFromJSON, GToJSON, Parser, Zero)
import GHC.Generics (Generic, Rep)
import Network.HTTP.Types (Status, hContentType, status200)
import Network.Wai (Request, Response, responseLBS, strictRequestBody)
import Web.Kamyu.Core (KamyuHandler)
import Web.Kamyu.Status (badRequest)

-- | Possible errors when we parse JSON request bodies
newtype JsonError = JsonDecodeError String deriving (Show, Eq)

-- | Parse request body as JSON using the provided target type
jsonBody :: (FromJSON body) => Request -> IO (Either JsonError body)
jsonBody req = do
  body <- strictRequestBody req
  case eitherDecode body of
    Left err -> pure (Left (JsonDecodeError err))
    Right val -> pure (Right val)

-- | Create a JSON response with `application/json` content type (defaults to 200)
jsonResponse :: (ToJSON a) => a -> Response
jsonResponse = jsonResponseWith status200

-- | Same as 'jsonResponse' but lets you pick a custom Status
jsonResponseWith :: (ToJSON a) => Status -> a -> Response
jsonResponseWith statusCode value =
  responseLBS statusCode [(hContentType, "application/json")] (encode value)

-- | JSON worker: takes parsed body, Request and path params.
jsonHandler ::
  (FromJSON body, ToJSON result) =>
  (body -> Request -> [(String, String)] -> IO (Status, result)) ->
  KamyuHandler
jsonHandler handler request pathParams = do
  parsed <- jsonBody request
  case parsed of
    Left err -> pure $ badRequest (jsonErrorMessage err)
    Right body -> do
      (statusCode, output) <- handler body request pathParams
      pure (jsonResponseWith statusCode output)

jsonErrorMessage :: JsonError -> String
jsonErrorMessage (JsonDecodeError msg) = "Failed to decode JSON: " ++ msg

-- | Type class, which generates parsers and serialization.
class JsonCodec a where
  toJson :: a -> Value
  default toJson :: (Generic a, GToJSON Zero (Rep a)) => a -> Value
  toJson = genericToJSON defaultOptions

  parseJson :: Value -> Parser a
  default parseJson :: (Generic a, GFromJSON Zero (Rep a)) => Value -> Parser a
  parseJson = genericParseJSON defaultOptions

instance {-# OVERLAPPABLE #-} (JsonCodec a) => ToJSON a where
  toJSON = toJson

instance {-# OVERLAPPABLE #-} (JsonCodec a) => FromJSON a where
  parseJSON = parseJson
