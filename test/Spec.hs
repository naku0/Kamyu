{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

import GHC.Generics (Generic)
import Test.Hspec
import Test.Hspec.Wai
import Network.Wai (Application, Middleware, mapResponseHeaders)
import Web.Kamyu.Core hiding (Middleware)
import Web.Kamyu.Json (JsonCodec, jsonCreate)
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

data CreateUser = CreateUser
    { createUserName :: String
    }
    deriving (Show, Generic, JsonCodec)

data User = User
    { userId :: Int,
      userName :: String
    }
    deriving (Show, Generic, JsonCodec)

createUser :: CreateUser -> IO User
createUser CreateUser {createUserName = name} =
    pure $ User 1 name

testRoutes :: KamyuBuilder
testRoutes = do
    Kamyu.createMiddleware testMiddleware

    Kamyu.get "/" $ \_ _ -> do
        return $ ok $ "SUCCESS! Kamyu is working!"

    Kamyu.get "/home" homeHandler

    Kamyu.get "/search" $ \req _ -> do
        let query = getString "q" req `orElse` ""
            page = getInt "page" req `orElse` 1
        return $ ok $ "Search: " ++ query ++ ", page: " ++ show page

    Kamyu.path "api" $ do
        Kamyu.get "/status" $ \_ _ -> do
            return $ ok "API is alive"

        Kamyu.path "users" $
            Kamyu.capture "id" $
                Kamyu.get "/profile" $ \_ params -> do
                    let profileId = lookup "id" params `orElse` "unknown"
                    return $ ok $ "Profile: " ++ profileId

        Kamyu.root $
            Kamyu.get "/outside" $ \_ _ -> do
                return $ ok "Outside api"

    Kamyu.post "/users" $ jsonCreate createUser

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

        describe "Nested route combinators" $ do
            it "prefixes routes with path" $ do
                get "/api/status" `shouldRespondWith` "API is alive" { matchStatus = 200 }

            it "captures dynamic path segments" $ do
                get "/api/users/42/profile" `shouldRespondWith` "Profile: 42" { matchStatus = 200 }

            it "can temporarily return to root context" $ do
                get "/outside" `shouldRespondWith` "Outside api" { matchStatus = 200 }

            it "does not register root routes under the previous path context" $ do
                get "/api/outside" `shouldRespondWith` 404

        describe "JSON helpers" $ do
            it "creates a resource from a JSON body" $ do
                request "POST" "/users" [("Content-Type", "application/json")] "{\"createUserName\":\"Alice\"}"
                    `shouldRespondWith` "{\"userId\":1,\"userName\":\"Alice\"}" { matchStatus = 201 }
