module Web.Kamyu.Status
  ( ok,
    badRequest,
    notFound,
    unauthorized,
  )
where

import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Network.HTTP.Types (status200, status400, status403, status404)
import Network.Wai (Response, responseLBS)

ok :: String -> Response
ok = responseLBS status200 [] . toLBS

notFound :: String -> Response
notFound = responseLBS status404 [] . toLBS

badRequest :: String -> Response
badRequest = responseLBS status400 [] . toLBS

unauthorized :: String -> Response
unauthorized = responseLBS status403 [] . toLBS

toLBS :: String -> LBS.ByteString
toLBS = LBS.fromStrict . T.encodeUtf8 . T.pack
