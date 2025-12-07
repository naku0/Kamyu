{-# LANGUAGE OverloadedStrings #-}

module Web.Kamyu.DSL
    ( module Web.Kamyu.Params
    , require
    , optional
    , paginate
    , withQuery
    , getInt, getText, getBool, getDouble
    , getIntDef, getTextDef, getBoolDef, getDoubleDef
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
require parser name handler req =
    case parser req name of
        Right val -> handler val req
        Left err -> return $ badRequest (show err)

-- | Optional parameter (handler receives Maybe)
optional :: (Request -> String -> Maybe a)
         -> String
         -> (Maybe a -> KamyuHandler)
         -> KamyuHandler
optional getter name handler req =
    handler (getter req name) req

-- | Handle paginated requests
paginate :: (Int -> Int -> KamyuHandler) -> KamyuHandler
paginate handler req = do
    let pageNum = intDef 1 "page" req
        pageSize = intDef 20 "limit" req
    handler pageNum pageSize req

-- | Handle search with query
withQuery :: (String -> KamyuHandler) -> KamyuHandler
withQuery handler req = do
    let queryText = textDef "" "q" req
    handler queryText req

getInt :: String -> Request -> Either ParamError Int
getInt = int

getText :: String -> Request -> Either ParamError String
getText = text

getBool :: String -> Request -> Either ParamError Bool
getBool = bool

getDouble :: String -> Request -> Either ParamError Double  
getDouble = double

getIntDef :: Int -> String -> Request -> Int
getIntDef = intDef

getTextDef :: String -> String -> Request -> String
getTextDef = textDef

getBoolDef :: Bool -> String -> Request -> Bool
getBoolDef = boolDef

getDoubleDef :: Double -> String -> Request -> Double
getDoubleDef = doubleDef