module Web.Kamyu.Router
  ( addRoute,
    addMiddleware,
    withPathContext,
    withRootContext,
    matchRoute,
  )
where

import Control.Monad.Trans.State (get, put)
import Data.List (intercalate, isPrefixOf)
import Data.Text (Text)
import qualified Data.Text as T
import Web.Kamyu.Core
  ( Kamyu (Kamyu),
    KamyuHandler,
    KamyuState (middlewareChain, pathContext, routes),
    Method,
    Middleware,
    PathSegment (..),
    Route (Route),
  )

addRoute :: Method -> String -> KamyuHandler -> Kamyu ()
addRoute method pathPattern handler = do
  currentState <- getKamyuState
  let fullPath = buildFullPath (pathContext currentState) pathPattern
      pat = parsePathPattern fullPath
      newRoute = Route method pat handler
  putKamyuState $ currentState {routes = newRoute : routes currentState}

addMiddleware :: Middleware -> Kamyu ()
addMiddleware mw = do
  currentState <- getKamyuState
  let existing = middlewareChain currentState
  putKamyuState $ currentState {middlewareChain = existing <> [mw]}

withPathContext :: String -> Kamyu a -> Kamyu a
withPathContext pathPart (Kamyu action) = Kamyu $ do
  currentState <- get
  let previousContext = pathContext currentState
      nextContext = previousContext <> splitPath pathPart
  put currentState {pathContext = nextContext}
  result <- action
  updatedState <- get
  put updatedState {pathContext = previousContext}
  pure result

withRootContext :: Kamyu a -> Kamyu a
withRootContext (Kamyu action) = Kamyu $ do
  currentState <- get
  let previousContext = pathContext currentState
  put currentState {pathContext = []}
  result <- action
  updatedState <- get
  put updatedState {pathContext = previousContext}
  pure result

matchRoute :: [Text] -> [PathSegment] -> Maybe [(String, String)]
matchRoute [] [] = Just []
matchRoute [] _ = Nothing
matchRoute _ [] = Nothing
matchRoute (reqPart : reqParts) (Static static : patternParts)
  | T.unpack reqPart == static = matchRoute reqParts patternParts
  | otherwise = Nothing
matchRoute (reqPart : reqParts) (Dynamic paramName : patternParts) = do
  rest <- matchRoute reqParts patternParts
  return ((paramName, T.unpack reqPart) : rest)

getKamyuState :: Kamyu KamyuState
getKamyuState = Kamyu get

putKamyuState :: KamyuState -> Kamyu ()
putKamyuState = Kamyu . put

parsePathPattern :: String -> [PathSegment]
parsePathPattern = map parseSegment . splitPath
  where
    parseSegment seg
      | ":" `isPrefixOf` seg = Dynamic (drop 1 seg)
      | otherwise = Static seg

splitPath :: String -> [String]
splitPath "" = []
splitPath "/" = []
splitPath path = filter (not . null) (splitOn '/' path)

splitOn :: Char -> String -> [String]
splitOn delimiter = go
  where
    go "" = []
    go s =
      let (part, rest) = break (== delimiter) s
          rest' = dropWhile (== delimiter) rest
       in part : go rest'

buildFullPath :: [String] -> String -> String
buildFullPath context pathStr =
  "/" ++ intercalate "/" (context <> splitPath pathStr)
