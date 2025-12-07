{-# LANGUAGE OverloadedStrings #-}

module Main(main) where

import Web.Kamyu.Server (runKamyu)
import Web.Kamyu.Combinators ( get )
import Web.Kamyu.Status (ok)
import Web.Kamyu.Core (KamyuHandler)
import Web.Kamyu.Params (textDef)

homeHandler :: KamyuHandler
homeHandler _ = do
     return $ ok "Home is here"

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

        