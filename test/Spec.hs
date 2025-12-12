{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON
import Network.Wai (Application, Request(..), Response, requestMethod, pathInfo)
import Network.HTTP.Types (status200)
import Web.Kamyu.Core
import qualified Web.Kamyu.Combinators as Kamyu
import Web.Kamyu.Status (ok, notFound)
import Web.Kamyu.Params (getString, getInt, orElse)
import Control.Monad.Trans.State (runStateT)
import Control.Monad.Trans.Except (runExceptT)
import Data.List (find)
import Data.Maybe (isJust)
import qualified Data.ByteString.Char8 as BS

-- Re-implementation of runKamyuApp from Server.hs since it's not exported in a usable way for testing
runKamyuApp :: KamyuState -> Kamyu a -> IO (Either KamyuError (a, KamyuState))
runKamyuApp state (Kamyu action) = runExceptT (runStateT action state)

-- Re-implementation of createApp logic from Server.hs
createApp :: KamyuState -> Application
createApp state request respond = do
    let matching = findMatchingRoute (routes state) request
    case matching of
        Just (handler, params, _) -> do
            response <- handler request params
            respond response
        Nothing -> do
            respond $ notFound "Not found 404"

findMatchingRoute :: [Route] -> Request -> Maybe (KamyuHandler, [(String, String)], [PathSegment])
findMatchingRoute routes' request = 
    find (matchesRoute request) routes' >>= extractRouteInfo
  where
    matchesRoute request route = 
        show (routeMethod route) == BS.unpack (requestMethod request) &&
        isJust (matchRoute (pathInfo request) (routePattern route))
    
    extractRouteInfo route = 
        case matchRoute (pathInfo request) (routePattern route) of
            Just params -> Just (routeHandler route, params, routePattern route)
            Nothing -> Nothing

-- Re-implementation of handlers from Main.hs
homeHandler :: KamyuHandler
homeHandler _ _ = return $ ok $ "Home is here"

testRoutes :: KamyuBuilder
testRoutes = do
    Kamyu.get "/" $ \_ _ -> do
        return $ ok $ "SUCCESS! Kamyu is working!"

    Kamyu.get "/home" homeHandler

    Kamyu.get "/search" $ \req params -> do
        let query = getString "q" req `orElse` ""
            page = getInt "page" req `orElse` 1
        return $ ok $ "Search: " ++ query ++ ", page: " ++ show page

-- Build the application for testing
buildTestApp :: IO Application
buildTestApp = do
    let initialState = KamyuState [] [] []
    result <- runKamyuApp initialState testRoutes
    case result of
        Left err -> error $ "Kamyu error: " ++ show err
        Right (_, finalState) -> return $ createApp finalState

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
