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
    ) where

import Control.Monad.Trans.State
import Control.Monad.Trans.Except
import Network.Wai (Request, Response, Application)
import Control.Monad.IO.Class (MonadIO)

data Method = GET | POST | PUT | DELETE | PATCH deriving (Show, Eq)
type KamyuHandler = Request -> IO Response
type Middleware = Application -> Application

data Route = Route 
    { routeMethod :: Method
    , routePath :: [String]
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

getKamyuState :: Kamyu KamyuState
getKamyuState = Kamyu get

putKamyuState :: KamyuState -> Kamyu ()
putKamyuState = Kamyu . put

type KamyuApp = Application 
type KamyuBuilder = Kamyu ()

addRoute :: Method -> String -> KamyuHandler -> Kamyu ()
addRoute method pathPattern handler = do
    currentState <- getKamyuState
    let fullPath = buildFullPath (pathContext currentState) pathPattern
        newRoute = Route method fullPath handler
    putKamyuState $ currentState {routes = newRoute : routes currentState}

buildFullPath :: [String] -> String -> [String]
buildFullPath context pathStr = context <> filter (not . null) (splitPath pathStr)

splitPath :: String -> [String]
splitPath = filter (not . null) . splitOn '/'
    where 
        splitOn :: Char -> String -> [String]
        splitOn delimiter = foldr f [""]
            where
                f c [s]    | c  == delimiter = ["", s]
                f c (s:ss) | c  == delimiter = "":s:ss
                f c (s:ss)                   = (c:s):ss