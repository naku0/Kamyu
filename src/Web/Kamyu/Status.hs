module Web.Kamyu.Status(
    ok,
    badRequest, notFound
    ) where

import Network.Wai (responseLBS, Response)
import Network.HTTP.Types (status200, status400, status404)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

ok :: String -> Response
ok = responseLBS status200 [] . toLBS

notFound :: String -> Response
notFound = responseLBS status404 [] . toLBS

badRequest :: String -> Response
badRequest = responseLBS status400 [] .toLBS

toLBS :: String -> LBS.ByteString
toLBS = LBS.fromStrict . T.encodeUtf8 . T.pack


