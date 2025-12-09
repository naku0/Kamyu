{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Main(main) where

import Web.Kamyu.Server (runKamyu)
import Web.Kamyu.Combinators ( get, post )
import Web.Kamyu.Status (ok)
import Web.Kamyu.Core (KamyuHandler)
import Web.Kamyu.Params (textDef)
import Web.Kamyu.Json (jsonWithStatus)
import GHC.Generics (Generic)
import Data.Aeson (FromJSON, ToJSON)
import Network.HTTP.Types (Status, status201)

homeHandler :: KamyuHandler
homeHandler _ = do
     return $ ok "Home is here"

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

main :: IO ()
main = do
    putStrLn "=== KAMYU START ==="
    runKamyu 8080 $ do

        get "/" $ \_ -> do
            putStrLn "⭐ Handler for GET / called!"
            return $ ok "SUCCESS! Kamyu is working!"

        get "/home" homeHandler

        get "/hello" $ \req -> do
            let name = textDef "World" "name" req
            return $ ok $ "Hello, " ++ name ++ "!"

        post "/people" $ jsonWithStatus createPerson

        
