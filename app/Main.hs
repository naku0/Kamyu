{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE BlockArguments #-}

module Main(main) where

import Web.Kamyu.Server (runKamyu)
import Web.Kamyu.Combinators ( get, post, middleware )
import Web.Kamyu.Status (ok, unauthorized)
import Web.Kamyu.Core (KamyuHandler, Middleware)
import Web.Kamyu.Json (jsonHandler, JsonCodec)
import GHC.Generics (Generic)
import Network.HTTP.Types (Status, status201)
import Network.Wai ( Request
                   , requestMethod
                   , pathInfo
                   , mapResponseHeaders
                   , requestHeaders)
import Web.Kamyu.Params (orElse, getInt, getString, pathParamDef)
import qualified Data.ByteString.Char8 as BS
import qualified Data.CaseInsensitive as CI

homeHandler :: KamyuHandler
homeHandler _ _ = return $ ok $ "Home is here"

data CreatePerson = CreatePerson
    { name :: String
    , age :: Int
    } deriving (Show, Generic, JsonCodec)

data Person = Person
    { identifier :: Int
    , fullName :: String
    , personAge :: Int
    } deriving (Show, Generic, JsonCodec)

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
        Just header | "Bearer " `BS.isPrefixOf` header
                    , let token = BS.drop 7 header
                    , isAllowed token -> app req respond
        _ -> respond $ unauthorized "Missing or invalid token"

createPerson :: CreatePerson -> Request -> [(String, String)] -> IO (Status, Person)
createPerson CreatePerson{name = personName, age = personAgeVal} req pathParams = do
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
            return $ ok $ "SUCCESS! Kamyu is working!"

        get "/home" homeHandler

        get "/user/:id" $ \_ params -> do
            let userId = pathParamDef "0" "id" params
            return $ ok $ "User ID: " ++ userId

        get "/search" $ \req _ -> do
            let query = getString "q" req `orElse` ""
                page = getInt "page" req `orElse` 1
            return $ ok $ "Search: " ++ query ++ ", page: " ++ show page

        post "/cities/:city/people" $ jsonHandler createPerson
