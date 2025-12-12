{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import GHC.Generics (Generic)
import Network.HTTP.Types (Status, status201)
import Network.Wai (Request)
import Web.Kamyu

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
    createMiddleware $
      buildMiddleware $
        use (auth (== "super-secret"))
          |> exceptRoute "/"
          |> exceptRoute "/health"
          |> exceptRoute "/home"

    createMiddleware $
      buildMiddleware $
        use cors
          |> forRoute "/api/*"
          |> forRoute "/cities/*"

    createMiddleware $ buildMiddleware $ use (poweredBy "Kamyu")

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
