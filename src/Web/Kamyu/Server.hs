{-# LANGUAGE OverloadedStrings #-}

module Web.Kamyu.Server(
    runKamyu
    ) where

import Web.Kamyu.Core
    ( KamyuBuilder,
      Kamyu(Kamyu),
      KamyuError,
      KamyuState(KamyuState, routes),
      Route(routePath, routeHandler, routeMethod),
      KamyuHandler )
import Network.Wai
    ( Request(pathInfo, requestMethod), Application )
import Network.Wai.Handler.Warp (run)
import Data.List (find)
import qualified Data.Text as T
import Data.Maybe (isJust)
import Control.Monad.Trans.State (runStateT)
import Control.Monad.Trans.Except (runExceptT)
import qualified Data.ByteString.Char8 as BS
import Web.Kamyu.Status (notFound)

runKamyu :: Int -> KamyuBuilder -> IO ()
runKamyu port builder = do
    putStrLn "🚀 Starting Kamyu server..."
    let initialState = KamyuState [] [] []
    result <- runKamyuApp initialState builder
    case result of
        Left err -> error $ "Kamyu error: " ++ show err
        Right (_, finalState) -> do
            putStrLn "📊 Registered routes:"
            mapM_ printRoute (routes finalState)
            let app = createApp finalState
            putStrLn $ "🌐 Server running on port " ++ show port
            run port app
  where
    printRoute route = putStrLn $ "  " ++ show (routeMethod route) ++ " " ++ show (routePath route)

runKamyuApp :: KamyuState -> Kamyu a -> IO (Either KamyuError (a, KamyuState))
runKamyuApp state (Kamyu action) = runExceptT (runStateT action state)

createApp :: KamyuState -> Application
createApp state request respond = do
    putStrLn $ "📨 Request: " ++ show (requestMethod request) ++ " " ++ show (pathInfo request)
    putStrLn $ "📋 Available routes: " ++ show (map (\r -> (routeMethod r, routePath r)) (routes state))
    
    let matching = findMatchingRoute (routes state) request
    putStrLn $ "🔍 Match result: " ++ show (isJust matching)
    
    case matching of
        Just handler -> do
            putStrLn "✅ Calling handler"
            response <- handler request
            respond response
        Nothing -> do
            putStrLn "❌ No handler found"
            respond $ notFound "Not found 404"


findMatchingRoute :: [Route] -> Request -> Maybe KamyuHandler
findMatchingRoute routes' request = find (`matchesRoute` request) routes' >>= Just . routeHandler

matchesRoute :: Route -> Request -> Bool
matchesRoute route request =
    let methodMatches = show (routeMethod route) == BS.unpack (requestMethod request)
        pathMatches = map T.unpack (pathInfo request) == routePath route
    in methodMatches && pathMatches
{-
matchesRoute :: Route -> Request -> Bool
matchesRoute route request =
    methodMatches && pathMatches
  where
    methodMatches = show (routeMethod route) == show (requestMethod request)
    pathMatches = pathInfo request == map T.pack (routePath route)
    -}