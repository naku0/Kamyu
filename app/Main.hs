{-# LANGUAGE OverloadedStrings #-}

module Main(main) where

import Web.Kamyu.Server (runKamyu)
import Web.Kamyu.Combinators ( get )
import Web.Kamyu.Status (ok)
import Web.Kamyu.Core (KamyuHandler)

homeHandler :: KamyuHandler
homeHandler _ = do
     putStrLn "Calling home"
     return $ ok "Home is here"

main :: IO ()
main = do
    putStrLn "=== KAMYU START ==="
    runKamyu 8080 $ do

        get "/" $ \_ -> do
            putStrLn "⭐ Handler for GET / called!"
            return $ ok "SUCCESS! Kamyu is working!"

        get "/home" homeHandler


