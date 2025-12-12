{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Web.Kamyu.Core
    ( Method(..)
    , KamyuHandler
    , Middleware
    , Route(..)
    , KamyuState(..)
    , KamyuError(..)
    , Kamyu(..)
    , KamyuApp
    , KamyuBuilder
    , addRoute
    , addMiddleware
    , PathSegment(..)
    , matchRoute
    ) where

import Control.Monad.Trans.State
import Control.Monad.Trans.Except
import Network.Wai (Request, Response, Application)
import Control.Monad.IO.Class (MonadIO)
import Data.Text (Text)
import qualified Data.Text as T
import Data.List (isPrefixOf, intercalate)

data Method = GET | POST | PUT | DELETE | PATCH deriving (Show, Eq)
type KamyuHandler = Request -> [(String, String)] -> IO Response
type Middleware = Application -> Application

data PathSegment
    = Static String
    | Dynamic String
    deriving (Show, Eq)

data Route = Route 
    { routeMethod :: Method
    , routePattern :: [PathSegment]
    , routeHandler :: KamyuHandler
    }

data KamyuState = KamyuState 
    { routes :: [Route]
    , pathContext :: [String]
    , middlewareChain :: [Middleware]
    }

data KamyuError = RouteConflict String | InvalidPath String deriving (Show)

newtype Kamyu a = Kamyu 
    { unKamyu :: StateT KamyuState (ExceptT KamyuError IO) a 
    } deriving (Functor, Applicative, Monad, MonadIO)

type KamyuApp = Application 
type KamyuBuilder = Kamyu ()

-- | Получить текущее состояние
getKamyuState :: Kamyu KamyuState
getKamyuState = Kamyu get

-- | Установить новое состояние
putKamyuState :: KamyuState -> Kamyu ()
putKamyuState = Kamyu . put

-- | Парсинг строки пути в паттерн
parsePathPattern :: String -> [PathSegment]
parsePathPattern = map parseSegment . splitPath
  where
    parseSegment :: String -> PathSegment
    parseSegment seg
        | ":" `isPrefixOf` seg = Dynamic (drop 1 seg)
        | otherwise = Static seg

-- | Разделение пути (исправленная версия)
splitPath :: String -> [String]
splitPath "" = []  -- Пустой путь
splitPath "/" = []  -- Корневой путь
splitPath path = filter (not . null) (splitOn '/' path)
  where
    splitOn :: Char -> String -> [String]
    splitOn delimiter = go
      where
        go "" = []
        go s = 
            let (part, rest) = break (== delimiter) s
                rest' = dropWhile (== delimiter) rest
            in part : go rest'

----------------------------------------------------------------
-- ОБНОВЛЕННЫЙ addRoute
----------------------------------------------------------------

addRoute :: Method -> String -> KamyuHandler -> Kamyu ()
addRoute method pathPattern handler = do
    currentState <- getKamyuState
    let fullPath = buildFullPath (pathContext currentState) pathPattern
        pattern = parsePathPattern fullPath
        newRoute = Route method pattern handler
    putKamyuState $ currentState { routes = newRoute : routes currentState }

addMiddleware :: Middleware -> Kamyu ()
addMiddleware mw = do
    currentState <- getKamyuState
    let existing = middlewareChain currentState
    putKamyuState $ currentState { middlewareChain = existing <> [mw] }

buildFullPath :: [String] -> String -> String
buildFullPath context pathStr = 
    let contextStr = if null context then "" else "/" ++ intercalate "/" context
        pathStr' = if "/" `isPrefixOf` pathStr then pathStr else "/" ++ pathStr
    in contextStr ++ pathStr'

-- | Проверить совпадение и извлечь параметры
matchRoute :: [Text] -> [PathSegment] -> Maybe [(String, String)]
matchRoute [] [] = Just []
matchRoute [] _ = Nothing
matchRoute _ [] = Nothing
matchRoute (reqPart:reqParts) (Static static:patternParts)
    | T.unpack reqPart == static = matchRoute reqParts patternParts
    | otherwise = Nothing
matchRoute (reqPart:reqParts) (Dynamic paramName:patternParts) = do
    rest <- matchRoute reqParts patternParts
    return ((paramName, T.unpack reqPart) : rest)
