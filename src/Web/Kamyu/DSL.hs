{-# LANGUAGE OverloadedStrings #-}

module Web.Kamyu.DSL
    ( module Web.Kamyu.Params
    , require
    , optional
    ) where

import Web.Kamyu.Params
import Web.Kamyu.Core
import Web.Kamyu.Status (badRequest)
import Network.Wai (Request)

-- | Require a parameter (throws 400 if missing/invalid)
require :: (Request -> String -> Either ParamError a)
        -> String
        -> (a -> KamyuHandler)
        -> KamyuHandler
require parser name handler req params =
    case parser req name of
        Right val -> handler val req params
        Left err -> return $ badRequest (show err)

-- | Optional parameter (handler receives Maybe)
optional :: (Request -> String -> Maybe a)
         -> String
         -> (Maybe a -> KamyuHandler)
         -> KamyuHandler
optional getter name handler req = handler (getter req name) req