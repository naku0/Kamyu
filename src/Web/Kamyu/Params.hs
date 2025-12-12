{-# LANGUAGE OverloadedStrings #-}

module Web.Kamyu.Params
  ( -- * Query parameter functions
    getString,
    getInt,
    getBool,
    getDouble,
    getStringOpt,
    getIntOpt,
    getBoolOpt,
    getDoubleOpt,
    getStringDef,
    getIntDef,
    getBoolDef,
    getDoubleDef,
    pathParam,
    pathParamDef,
    orElse,
    orMaybe,
    ParamError (..),

    -- * New simple DSL functions
    fromPath,
    fromPathInt,
    fromPathBool,
    fromPathDouble,
    fromQuery,
    fromQueryInt,
    fromQueryBool,
    fromQueryDouble,
    orDefault,
  )
where

import Data.Char (isUpper, toLower)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import qualified Data.Text.Read as TR
import Network.Wai (Request, queryString)

-- For user convenience, we accept String but work with Text internally
type ParamName = String

data ParamError
  = MissingParam ParamName
  | InvalidInt ParamName
  | InvalidDouble ParamName
  | InvalidBool ParamName
  deriving (Show, Eq)

-- Helper: Get raw parameter as Text
rawParam :: ParamName -> Request -> Maybe Text
rawParam key req =
  case lookup (encodeUtf8 (T.pack key)) (queryString req) of
    Just (Just val) -> Just (decodeUtf8 val)
    _ -> Nothing

----------------------------------------------------------------
-- PATH PARAMETER FUNCTIONS
----------------------------------------------------------------

-- | Get path parameter from list
pathParam :: String -> [(String, String)] -> Maybe String
pathParam = lookup

-- | Get path parameter with default value
pathParamDef :: String -> String -> [(String, String)] -> String
pathParamDef def name params = fromMaybe def (lookup name params)

----------------------------------------------------------------
-- QUERY PARAMETER FUNCTIONS (updated to use Maybe)
----------------------------------------------------------------

-- | Get required string parameter (returns Nothing if missing)
getString :: ParamName -> Request -> Maybe String
getString name req = do
  textVal <- rawParam name req
  Just (T.unpack textVal)

-- | Get required integer parameter (returns Nothing if missing or invalid)
getInt :: ParamName -> Request -> Maybe Int
getInt name req = do
  textVal <- rawParam name req
  case TR.decimal textVal of
    Right (val, rest) | T.null rest -> Just val
    _ -> Nothing

-- | Get required boolean parameter (returns Nothing if missing or invalid)
getBool :: ParamName -> Request -> Maybe Bool
getBool name req = do
  textVal <- rawParam name req
  let lower = T.unpack (T.toLower textVal)
  case lower of
    "true" -> Just True
    "false" -> Just False
    "1" -> Just True
    "0" -> Just False
    "yes" -> Just True
    "no" -> Just False
    _ -> Nothing

-- | Get required double parameter (returns Nothing if missing or invalid)
getDouble :: ParamName -> Request -> Maybe Double
getDouble name req = do
  textVal <- rawParam name req
  case TR.double textVal of
    Right (val, rest) | T.null rest -> Just val
    _ -> Nothing

----------------------------------------------------------------
-- OPTIONAL GETTERS (these are now the same as the regular getters)
----------------------------------------------------------------

-- | Get optional string parameter (same as getString)
getStringOpt :: ParamName -> Request -> Maybe String
getStringOpt = getString

-- | Get optional integer parameter (same as getInt)
getIntOpt :: ParamName -> Request -> Maybe Int
getIntOpt = getInt

-- | Get optional boolean parameter (same as getBool)
getBoolOpt :: ParamName -> Request -> Maybe Bool
getBoolOpt = getBool

-- | Get optional double parameter (same as getDouble)
getDoubleOpt :: ParamName -> Request -> Maybe Double
getDoubleOpt = getDouble

----------------------------------------------------------------
-- NEW SIMPLE DSL FUNCTIONS
----------------------------------------------------------------

-- | Get path parameter as Maybe String (new DSL)
fromPath :: String -> [(String, String)] -> Maybe String
fromPath = lookup

-- | Get path parameter as Maybe Int (new DSL)
fromPathInt :: String -> [(String, String)] -> Maybe Int
fromPathInt name params = do
  str <- lookup name params
  case TR.decimal (T.pack str) of
    Right (val, rest) | T.null rest -> Just val
    _ -> Nothing

-- | Get path parameter as Maybe Bool (new DSL)
fromPathBool :: String -> [(String, String)] -> Maybe Bool
fromPathBool name params = do
  str <- lookup name params
  let lower = map toLower str
  case lower of
    "true" -> Just True
    "false" -> Just False
    "1" -> Just True
    "0" -> Just False
    "yes" -> Just True
    "no" -> Just False
    _ -> Nothing
  where
    toLower c = if isUpper c then toEnum (fromEnum c + 32) else c

-- | Get path parameter as Maybe Double (new DSL)
fromPathDouble :: String -> [(String, String)] -> Maybe Double
fromPathDouble name params = do
  str <- lookup name params
  case TR.double (T.pack str) of
    Right (val, rest) | T.null rest -> Just val
    _ -> Nothing

-- | Get query parameter as Maybe String (new DSL)
fromQuery :: String -> Request -> Maybe String
fromQuery = getString -- Now just an alias

-- | Get query parameter as Maybe Int (new DSL)
fromQueryInt :: String -> Request -> Maybe Int
fromQueryInt = getInt -- Now just an alias

-- | Get query parameter as Maybe Bool (new DSL)
fromQueryBool :: String -> Request -> Maybe Bool
fromQueryBool = getBool -- Now just an alias

-- | Get query parameter as Maybe Double (new DSL)
fromQueryDouble :: String -> Request -> Maybe Double
fromQueryDouble = getDouble -- Now just an alias

----------------------------------------------------------------
-- HELPER FUNCTIONS
----------------------------------------------------------------

-- | Provide default value for Maybe
orMaybe :: Maybe a -> a -> a
orMaybe (Just val) _ = val
orMaybe Nothing def = def

-- | Alias for orMaybe
orDefault :: Maybe a -> a -> a
orDefault = orMaybe

-- | Another alias for orMaybe
orElse :: Maybe a -> a -> a
orElse = orMaybe

----------------------------------------------------------------
-- CONVENIENCE FUNCTIONS WITH DEFAULTS
----------------------------------------------------------------

-- | Get parameter with default
getStringDef :: String -> ParamName -> Request -> String
getStringDef def name req = fromMaybe def (getString name req)

getIntDef :: Int -> ParamName -> Request -> Int
getIntDef def name req = fromMaybe def (getInt name req)

getBoolDef :: Bool -> ParamName -> Request -> Bool
getBoolDef def name req = fromMaybe def (getBool name req)

getDoubleDef :: Double -> ParamName -> Request -> Double
getDoubleDef def name req = fromMaybe def (getDouble name req)
