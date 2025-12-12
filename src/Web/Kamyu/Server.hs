{-# LANGUAGE OverloadedStrings #-}

module Web.Kamyu.Server
  ( runKamyu,
    kamyuApp,
  )
where

import Control.Monad.Trans.Except (runExceptT)
import Control.Monad.Trans.State (runStateT)
import qualified Data.ByteString.Char8 as BS
import Data.List (find)
import Data.Maybe (isJust)
import Network.Wai
  ( Application,
    Request (pathInfo, requestMethod),
  )
import Network.Wai.Handler.Warp (run)
import Web.Kamyu.Core
  ( Kamyu (Kamyu),
    KamyuBuilder,
    KamyuError,
    KamyuHandler,
    KamyuState (KamyuState, middlewareChain, routes),
    Method (..),
    Middleware,
    PathSegment (..),
    Route (routeHandler, routeMethod, routePattern),
    matchRoute,
  )
import Web.Kamyu.Status (notFound)

kamyuApp :: KamyuBuilder -> IO Application
kamyuApp builder = do
  let initialState = KamyuState [] [] []
  result <- runKamyuApp initialState builder
  case result of
    Left err -> error $ "Kamyu error: " ++ show err
    Right (_, finalState) -> return $ createApp finalState

runKamyu :: Int -> KamyuBuilder -> IO ()
runKamyu port builder = do
  putStrLn "🚀 Starting Kamyu server..."
  app <- kamyuApp builder
  let initialState = KamyuState [] [] []
  result <- runKamyuApp initialState builder
  case result of
    Left err -> error $ "Kamyu error: " ++ show err
    Right (_, finalState) -> do
      putStrLn "📊 Registered routes:"
      mapM_ printRoute (routes finalState)
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
createApp state = applyMiddlewares (middlewareChain state) (routerApp state)

routerApp :: KamyuState -> Application
routerApp state request respond = do
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

applyMiddlewares :: [Middleware] -> Application -> Application
applyMiddlewares mws app = foldr (\mw acc -> mw acc) app mws

findMatchingRoute :: [Route] -> Request -> Maybe (KamyuHandler, [(String, String)], [PathSegment])
findMatchingRoute routes' request =
  find (matchesRoute request) routes' >>= extractRouteInfo
  where
    matchesRoute req route =
      show (routeMethod route) == BS.unpack (requestMethod req)
        && isJust (matchRoute (pathInfo req) (routePattern route))

    extractRouteInfo route =
      case matchRoute (pathInfo request) (routePattern route) of
        Just params -> Just (routeHandler route, params, routePattern route)
        Nothing -> Nothing
