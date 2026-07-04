{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Web.Kamyu.Core
  ( Method (..),
    KamyuHandler,
    Middleware,
    Route (..),
    KamyuState (..),
    KamyuError (..),
    Kamyu (..),
    KamyuApp,
    KamyuBuilder,
    PathSegment (..),
  )
where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Except
import Control.Monad.Trans.State
import Network.Wai (Application, Request, Response)

data Method = GET | POST | PUT | DELETE | PATCH deriving (Show, Eq)

type KamyuHandler = Request -> [(String, String)] -> IO Response

type Middleware = Application -> Application

data PathSegment
  = Static String
  | Dynamic String
  deriving (Show, Eq)

data Route = Route
  { routeMethod :: Method,
    routePattern :: [PathSegment],
    routeHandler :: KamyuHandler
  }

data KamyuState = KamyuState
  { routes :: [Route],
    pathContext :: [String],
    middlewareChain :: [Middleware]
  }

data KamyuError = RouteConflict String | InvalidPath String deriving (Show)

newtype Kamyu a = Kamyu
  { unKamyu :: StateT KamyuState (ExceptT KamyuError IO) a
  }
  deriving (Functor, Applicative, Monad, MonadIO)

type KamyuApp = Application

type KamyuBuilder = Kamyu ()
