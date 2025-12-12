{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Web.Kamyu.Middleware
  ( MiddlewareBuilder,
    use,
    onlyIf,
    unless,
    forRoute,
    exceptRoute,
    withMethod,
    getOnly,
    postOnly,
    putOnly,
    deleteOnly,
    buildMiddleware,
    (.&), -- Combine with AND
    (.|), -- Combine with OR
    (.>), -- Add condition
    (|>), -- Pipe style
    logger,
    auth,
    cors,
    poweredBy,
  )
where

import qualified Data.ByteString.Char8 as BS
import qualified Data.CaseInsensitive as CI
import Data.Char (toUpper)
import Data.List (intercalate, isPrefixOf)
import qualified Data.Text as T
import Network.HTTP.Types.Status (status200)
import Network.Wai
  ( Request,
    mapResponseHeaders,
    pathInfo,
    requestHeaders,
    requestMethod,
    responseLBS,
  )
import Web.Kamyu.Core (Middleware)
import Web.Kamyu.Status (unauthorized)

newtype MiddlewareBuilder = Builder
  { unBuilder :: (Request -> Bool) -> Middleware
  }

use :: Middleware -> MiddlewareBuilder
use mw = Builder $ const mw

onlyIf :: (Request -> Bool) -> MiddlewareBuilder -> MiddlewareBuilder
onlyIf pred (Builder builder) = Builder $ \cond ->
  conditionalMiddleware (\req -> pred req && cond req) (builder cond)

unless :: (Request -> Bool) -> MiddlewareBuilder -> MiddlewareBuilder
unless pred = onlyIf (not . pred)

forRoute :: String -> MiddlewareBuilder -> MiddlewareBuilder
forRoute route = onlyIf (matchesRoute route)

exceptRoute :: String -> MiddlewareBuilder -> MiddlewareBuilder
exceptRoute route = unless (matchesRoute route)

withMethod :: String -> MiddlewareBuilder -> MiddlewareBuilder
withMethod method = onlyIf (\req -> BS.unpack (requestMethod req) == map toUpper method)

getOnly :: MiddlewareBuilder -> MiddlewareBuilder
getOnly = withMethod "GET"

postOnly :: MiddlewareBuilder -> MiddlewareBuilder
postOnly = withMethod "POST"

putOnly :: MiddlewareBuilder -> MiddlewareBuilder
putOnly = withMethod "PUT"

deleteOnly :: MiddlewareBuilder -> MiddlewareBuilder
deleteOnly = withMethod "DELETE"

buildMiddleware :: MiddlewareBuilder -> Middleware
buildMiddleware (Builder builder) = builder (const True)

infixl 3 .& -- AND combine

infixl 2 .| -- OR combine

infixl 1 .> -- Add condition

infixl 1 |> -- Pipe style

(.&) :: MiddlewareBuilder -> MiddlewareBuilder -> MiddlewareBuilder
(Builder b1) .& (Builder b2) = Builder $ \cond ->
  let mw1 = b1 cond
      mw2 = b2 cond
      combined app = mw1 (mw2 app)
   in combined

(.|) :: MiddlewareBuilder -> MiddlewareBuilder -> MiddlewareBuilder
(Builder b1) .| (Builder b2) = Builder $ \cond ->
  let mw1 = b1 cond
      mw2 = b2 cond
      combined app req respond =
        if cond req
          then mw1 app req respond
          else mw2 app req respond
   in combined

(.>) :: MiddlewareBuilder -> (Request -> Bool) -> MiddlewareBuilder
builder .> cond = onlyIf cond builder

(|>) :: MiddlewareBuilder -> (MiddlewareBuilder -> MiddlewareBuilder) -> MiddlewareBuilder
builder |> modifier = modifier builder

conditionalMiddleware :: (Request -> Bool) -> Middleware -> Middleware
conditionalMiddleware cond mw app req respond =
  if cond req
    then mw app req respond
    else app req respond

matchesRoute :: String -> (Request -> Bool)
matchesRoute pat req
  | "*" `isPrefixOf` pat = patternMatch pat req
  | otherwise = exactRoute pat req

exactRoute :: String -> (Request -> Bool)
exactRoute pat req =
  let reqPath = "/" ++ intercalate "/" (map T.unpack (pathInfo req))
   in reqPath == normalizeRoute pat

patternMatch :: String -> (Request -> Bool)
patternMatch pat req =
  let pathSegments = map T.unpack (pathInfo req)
      patSegments = splitOn '/' (dropWhile (== '/') pat)
   in matchPattern patSegments pathSegments

matchPattern :: [String] -> [String] -> Bool
matchPattern [] [] = True
matchPattern ["*"] _ = True
matchPattern ("*" : rest) (_ : pathRest) = matchPattern rest pathRest
matchPattern (pat : rest) (seg : pathRest)
  | pat == seg = matchPattern rest pathRest
  | otherwise = False
matchPattern _ _ = False

normalizeRoute :: String -> String
normalizeRoute "" = "/"
normalizeRoute r
  | not ("/" `isPrefixOf` r) = '/' : r
  | otherwise = r

splitOn :: Char -> String -> [String]
splitOn delim = go
  where
    go "" = []
    go s =
      let (part, rest) = break (== delim) s
       in part : go (dropWhile (== delim) rest)

logger :: Middleware
logger app req respond = do
  putStrLn $ "🌐 " ++ BS.unpack (requestMethod req) ++ " " ++ show (pathInfo req)
  app req respond

auth :: (BS.ByteString -> Bool) -> Middleware
auth isAllowed app req respond =
  case lookup (CI.mk "Authorization") (requestHeaders req) of
    Just header
      | "Bearer " `BS.isPrefixOf` header,
        let token = BS.drop 7 header,
        isAllowed token ->
          app req respond
    _ -> respond $ unauthorized "Missing or invalid token"

cors :: Middleware
cors app req respond = do
  let addHeaders =
        (("Access-Control-Allow-Origin", "*") :)
          . (("Access-Control-Allow-Methods", "GET, POST, PUT, DELETE, PATCH, OPTIONS") :)
          . (("Access-Control-Allow-Headers", "Content-Type, Authorization") :)
          . (("Access-Control-Max-Age", "86400") :)

  if requestMethod req == "OPTIONS"
    then
      respond $
        responseLBS
          status200
          (addHeaders [])
          ""
    else do
      let addCorsHeaders = mapResponseHeaders addHeaders
      app req (respond . addCorsHeaders)

poweredBy :: String -> Middleware
poweredBy name app req respond =
  app req $ \resp -> respond (mapResponseHeaders (("X-Powered-By", BS.pack name) :) resp)
