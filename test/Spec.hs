{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON
import Network.Wai (Application, Request(..), Response, requestMethod, pathInfo, Middleware, mapResponseHeaders)
import Network.HTTP.Types (status200)
import Web.Kamyu.Core hiding (Middleware)
import Web.Kamyu.Server (kamyuApp)
import qualified Web.Kamyu.Combinators as Kamyu
import Web.Kamyu.Status (ok)
import Web.Kamyu.Params (getString, getInt, orElse)

homeHandler :: KamyuHandler
homeHandler _ _ = return $ ok $ "Home is here"

testMiddleware :: Middleware
testMiddleware app req respond = app req $ \res -> do
    let res' = mapResponseHeaders (("X-Test-Header", "KamyuTest") :) res
    respond res'

testRoutes :: KamyuBuilder
testRoutes = do
    addMiddleware testMiddleware

    Kamyu.get "/" $ \_ _ -> do
        return $ ok $ "SUCCESS! Kamyu is working!"

    Kamyu.get "/home" homeHandler

    Kamyu.get "/search" $ \req params -> do
        let query = getString "q" req `orElse` ""
            page = getInt "page" req `orElse` 1
        return $ ok $ "Search: " ++ query ++ ", page: " ++ show page

buildTestApp :: IO Application
buildTestApp = kamyuApp testRoutes

main :: IO ()
main = hspec $ with buildTestApp $ do
    describe "Kamyu App" $ do
        describe "GET /" $ do
            it "responds with 200 and success message" $ do
                get "/" `shouldRespondWith` "SUCCESS! Kamyu is working!" { matchStatus = 200 }

        describe "GET /home" $ do
            it "responds with 200 and home message" $ do
                get "/home" `shouldRespondWith` "Home is here" { matchStatus = 200 }

        describe "GET /search" $ do
            it "responds with default search params" $ do
                get "/search" `shouldRespondWith` "Search: , page: 1" { matchStatus = 200 }

            it "responds with provided query param" $ do
                get "/search?q=haskell" `shouldRespondWith` "Search: haskell, page: 1" { matchStatus = 200 }

            it "responds with provided page param" $ do
                get "/search?page=2" `shouldRespondWith` "Search: , page: 2" { matchStatus = 200 }

            it "responds with both params" $ do
                get "/search?q=test&page=5" `shouldRespondWith` "Search: test, page: 5" { matchStatus = 200 }

        describe "Middleware" $ do
            it "adds X-Test-Header to response" $ do
                get "/" `shouldRespondWith` 200 { matchHeaders = ["X-Test-Header" <:> "KamyuTest"] }
