module Web.Kamyu.Status(
    ok
    ) where

import Network.Wai (responseLBS, Response)
import Network.HTTP.Types (status200)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

ok :: String -> Response
ok = responseLBS status200 [] . toLBS

toLBS :: String -> LBS.ByteString
toLBS = LBS.fromStrict . T.encodeUtf8 . T.pack


