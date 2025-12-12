{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Data.ByteString.Char8 as BS
import qualified Data.CaseInsensitive as CI
import GHC.Generics (Generic)
import Network.HTTP.Types (Status, status201)
import Network.Wai
  ( Request,
    mapResponseHeaders,
    pathInfo,
    requestHeaders,
    requestMethod,
  )
import Web.Kamyu (pathParam)
import Web.Kamyu.Combinators (get, middleware, post)
import Web.Kamyu.Core (KamyuHandler, Middleware)
import Web.Kamyu.Json (JsonCodec, jsonHandler)
import Web.Kamyu.Params (fromPath, fromQuery, fromQueryInt, getInt, getString, orDefault, orElse, pathParamDef)
import Web.Kamyu.Server (runKamyu)
import Web.Kamyu.Status (ok, unauthorized)

homeHandler :: KamyuHandler
homeHandler _ _ = return $ ok "Home is here"

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

requestLogger :: Middleware
requestLogger app req respond = do
  putStrLn $ "🧱 [MW] " ++ BS.unpack (requestMethod req) ++ " " ++ show (pathInfo req)
  app req respond

poweredBy :: Middleware
poweredBy app req respond =
  app req $ \response -> respond (mapResponseHeaders (("X-Powered-By", "Kamyu") :) response)

bearerAuth :: (BS.ByteString -> Bool) -> Middleware
bearerAuth isAllowed app req respond =
  case lookup (CI.mk "Authorization") (requestHeaders req) of
    Just header
      | "Bearer " `BS.isPrefixOf` header,
        let token = BS.drop 7 header,
        isAllowed token ->
          app req respond
    _ -> respond $ unauthorized "Missing or invalid token"

createPerson :: CreatePerson -> Request -> [(String, String)] -> IO (Status, Person)
createPerson CreatePerson {name = personName, age = personAgeVal} req pathParams = do
  let city = pathParamDef "unknown" "city" pathParams
      source = orElse (getString "source" req) "api"
      decoratedName = personName ++ " from " ++ city
  putStrLn $ "👤 Creating person: " ++ decoratedName ++ " (source=" ++ source ++ ")"
  let payload = Person 1 decoratedName personAgeVal
  return (status201, payload)

main :: IO ()
main = do
  putStrLn "=== KAMYU START ==="
  runKamyu 8080 $ do
    middleware requestLogger
    middleware poweredBy
    middleware (bearerAuth (== "super-secret"))

    get "/" $ \_ _ -> do
      putStrLn "⭐ Handler for GET / called!"
      return $ ok "SUCCESS! Kamyu is working!"

    get "/home" homeHandler

    get "/user/:id" $ \_ params -> do
      let userId = fromPath "id" params `orDefault` "0"
      return $ ok $ "User ID: " ++ userId

    get "/search" $ \req _ -> do
      let query = fromQuery "q" req `orElse` ""
          page = fromQueryInt "page" req `orElse` 1
      return $ ok $ "Search: " ++ query ++ ", page: " ++ show page

    post "/cities/:city/people" $ jsonHandler createPerson

    get "/health" \_ _ -> do
      return $ ok "Server health"
