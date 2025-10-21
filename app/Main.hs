{-# LANGUAGE OverloadedStrings #-}

module Main(main) where

import Web.Kamyu.Server (runKamyu)
import Web.Kamyu.Combinators ( get )    
import Network.Wai (responseLBS)
import Network.HTTP.Types (status200)

main :: IO ()
main = do
    putStrLn "=== KAMYU START ==="
    runKamyu 8080 $ do
        get "/" $ \_ -> do
            putStrLn "⭐ Handler for GET / called!"
            return $ responseLBS status200 [] "SUCCESS! Kamyu is working! 🎉"
        get "/home" $ \_ -> do
            putStrLn "Calling home"
            return $ responseLBS status200 [] "Home is here"
        