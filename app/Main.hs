{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE BlockArguments #-}

module Main(main) where

import Web.Kamyu.Server (runKamyu)
import Web.Kamyu.Combinators ( get, post )
import Web.Kamyu.Status (ok)
import Web.Kamyu.Core (KamyuHandler)
import Web.Kamyu.Json (jsonWithStatus)
import GHC.Generics (Generic)
import Data.Aeson (FromJSON, ToJSON)
import Network.HTTP.Types (Status, status201)
import Data.Maybe (fromMaybe)
import Web.Kamyu.Params (orElse, getInt, getString)

homeHandler :: KamyuHandler
homeHandler _ _ = return $ ok $ "Home is here"

data CreatePerson = CreatePerson
    { name :: String
    , age :: Int
    } deriving (Show, Generic)

instance FromJSON CreatePerson

data Person = Person
    { identifier :: Int
    , fullName :: String
    , personAge :: Int
    } deriving (Show, Generic)

instance ToJSON Person

createPerson :: CreatePerson -> IO (Status, Person)
createPerson CreatePerson{name = personName, age = personAgeVal} = do
    putStrLn $ "👤 Creating person: " ++ personName
    let payload = Person 1 personName personAgeVal
    return (status201, payload)

pathParamDef :: String -> String -> [(String, String)] -> String
pathParamDef def key params = Data.Maybe.fromMaybe def (lookup key params)

main :: IO ()
main = do
    putStrLn "=== KAMYU START ==="
    runKamyu 8080 $ do
        get "/" $ \_ _ -> do
            putStrLn "⭐ Handler for GET / called!"
            return $ ok $ "SUCCESS! Kamyu is working!"

        get "/home" homeHandler

        get "/user/:id" $ \_ params -> do
            let userId = pathParamDef "0" "id" params
            return $ ok $ "User ID: " ++ userId

        get "/search" $ \req params -> do
            let query = getString "q" req `orElse` ""
                page = getInt "page" req `orElse` 1
            return $ ok $ "Search: " ++ query ++ ", page: " ++ show page

        post "/people" $ jsonWithStatus createPerson