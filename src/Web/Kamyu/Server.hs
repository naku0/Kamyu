{-# LANGUAGE OverloadedStrings #-}

module Web.Kamyu.Server(
    runKamyu
    ) where

import Web.Kamyu.Core
    ( KamyuBuilder,
      Kamyu(Kamyu),
      KamyuError,
      KamyuState(KamyuState, routes),
      Route(routePattern, routeHandler, routeMethod),
      KamyuHandler,
      matchRoute,
      PathSegment(..),
      Method(..) )
import Network.Wai
    ( Request(pathInfo, requestMethod), Application, Response )
import Network.Wai.Handler.Warp (run)
import Data.List (find)
import Control.Monad.Trans.State (runStateT)
import Control.Monad.Trans.Except (runExceptT)
import qualified Data.ByteString.Char8 as BS
import Web.Kamyu.Status (notFound)
import Data.Maybe (isJust)

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
    printRoute route = 
        let pathStr = case routePattern route of
                [] -> "/"
                segments -> concatMap showSegment segments
        in putStrLn $ "  " ++ show (routeMethod route) ++ " " ++ pathStr
    
    showSegment (Static s) = "/" ++ s
    showSegment (Dynamic s) = "/:" ++ s

runKamyuApp :: KamyuState -> Kamyu a -> IO (Either KamyuError (a, KamyuState))
runKamyuApp state (Kamyu action) = runExceptT (runStateT action state)

createApp :: KamyuState -> Application
createApp state request respond = do
    putStrLn $ "📨 Request: " ++ BS.unpack (requestMethod request) ++ " " ++ show (pathInfo request)
    
    let matching = findMatchingRoute (routes state) request
    
    case matching of
        Just (handler, params, routePatternInfo) -> do
            putStrLn $ "✅ Match! Pattern: " ++ show routePatternInfo ++ ", Params: " ++ show params
            response <- handler request params
            respond response
        Nothing -> do
            putStrLn "❌ No match"
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