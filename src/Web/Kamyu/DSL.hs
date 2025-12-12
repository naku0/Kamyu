module Web.Kamyu.DSL
  ( module Web.Kamyu.Params,
    require,
    optional,
    handler,
    module Web.Kamyu.Status,
    module Web.Kamyu.Core,
  )
where

import Data.Aeson (ToJSON, encode)
import qualified Data.ByteString.Lazy as LBS
import Network.HTTP.Types (status200, status400)
import Network.Wai (Request, Response)
import Web.Kamyu.Core (KamyuHandler)
import Web.Kamyu.Params
import Web.Kamyu.Status (badRequest, created, notFound, ok, unauthorized)

----------------------------------------------------------------
-- HANDLER COMBINATORS
----------------------------------------------------------------

-- | Require a parameter (throws 400 if missing/invalid)
require ::
  (Request -> String -> Either ParamError a) ->
  String ->
  (a -> KamyuHandler) ->
  KamyuHandler
require parser name handler req params =
  case parser req name of
    Right val -> handler val req params
    Left err -> return $ badRequest (show err)

-- | Optional parameter (handler receives Maybe)
optional ::
  (Request -> String -> Maybe a) ->
  String ->
  (Maybe a -> KamyuHandler) ->
  KamyuHandler
optional getter name handler req = handler (getter req name) req

----------------------------------------------------------------
-- SIMPLE HANDLER BUILDER
----------------------------------------------------------------

-- | Build a simple handler with parameters
handler :: ((Request, [(String, String)]) -> IO Response) -> KamyuHandler
handler f req params = f (req, params)
