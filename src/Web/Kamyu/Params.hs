module Web.Kamyu.Params
  ( getString,
    getInt,
    getBool,
    getDouble,
    getStringOpt,
    getIntOpt,
    getBoolOpt,
    getDoubleOpt,
    pathParam,
    pathParamDef,
    orElse,
    orMaybe,
    defaultTo,
    ParamError (..),
  )
where

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
-- PATH PARAMETER GETTERS (НОВЫЕ!)
----------------------------------------------------------------

-- | Get path parameter from list
pathParam :: String -> [(String, String)] -> Maybe String
pathParam = lookup

-- | Get path parameter with default value
pathParamDef :: String -> String -> [(String, String)] -> String
pathParamDef def name params = fromMaybe def (lookup name params)

----------------------------------------------------------------
-- QUERY PARAMETER GETTERS (старые, без изменений)
----------------------------------------------------------------

-- | Get required string parameter
getString :: ParamName -> Request -> Either ParamError String
getString name req =
  case rawParam name req of
    Just val -> Right (T.unpack val)
    Nothing -> Left (MissingParam name)

-- | Get required integer parameter
getInt :: ParamName -> Request -> Either ParamError Int
getInt name req = do
  txt <- maybe (Left $ MissingParam name) Right (rawParam name req)
  case TR.decimal txt of
    Right (val, "") -> Right val
    _ -> Left (InvalidInt name)

-- | Get required boolean parameter
getBool :: ParamName -> Request -> Either ParamError Bool
getBool name req = do
  textVal <- maybe (Left $ MissingParam name) Right (rawParam name req)
  let lower = T.toLower textVal
  case lower of
    "true" -> Right True
    "false" -> Right False
    "1" -> Right True
    "0" -> Right False
    "yes" -> Right True
    "no" -> Right False
    _ -> Left (InvalidBool name)

-- | Get required double parameter
getDouble :: ParamName -> Request -> Either ParamError Double
getDouble name req = do
  textVal <- maybe (Left $ MissingParam name) Right (rawParam name req)
  case TR.double textVal of
    Right (val, "") -> Right val
    _ -> Left (InvalidDouble name)

----------------------------------------------------------------
-- OPTIONAL GETTERS
----------------------------------------------------------------

-- | Get optional string parameter
getStringOpt :: ParamName -> Request -> Maybe String
getStringOpt name req = do
  textVal <- rawParam name req
  Just (T.unpack textVal)

-- | Get optional integer parameter
getIntOpt :: ParamName -> Request -> Maybe Int
getIntOpt name req = do
  textVal <- rawParam name req
  case TR.decimal textVal of
    Right (val, "") -> Just val
    _ -> Nothing

-- | Get optional boolean parameter
getBoolOpt :: ParamName -> Request -> Maybe Bool
getBoolOpt name req = do
  textVal <- rawParam name req
  let lower = T.toLower textVal
  case lower of
    "true" -> Just True
    "false" -> Just False
    "1" -> Just True
    "0" -> Just False
    "yes" -> Just True
    "no" -> Just False
    _ -> Nothing

-- | Get optional double parameter
getDoubleOpt :: ParamName -> Request -> Maybe Double
getDoubleOpt name req = do
  textVal <- rawParam name req
  case TR.double textVal of
    Right (val, "") -> Just val
    _ -> Nothing

----------------------------------------------------------------
-- ERROR HANDLING / DEFAULT VALUES
----------------------------------------------------------------

-- | Provide default value for Either
orElse :: Either ParamError a -> a -> a
orElse (Right val) _ = val
orElse (Left _) def = def

-- | Provide default value for Maybe
orMaybe :: Maybe a -> a -> a
orMaybe (Just val) _ = val
orMaybe Nothing def = def

-- | Alias for orElse
defaultTo :: Either ParamError a -> a -> a
defaultTo = orElse

----------------------------------------------------------------
-- CONVENIENCE FUNCTIONS
----------------------------------------------------------------

-- | Get parameter with default (для тех кто предпочитает один вызов)
getStringDef :: String -> ParamName -> Request -> String
getStringDef def name req = fromMaybe def (getStringOpt name req)

getIntDef :: Int -> ParamName -> Request -> Int
getIntDef def name req = fromMaybe def (getIntOpt name req)

getBoolDef :: Bool -> ParamName -> Request -> Bool
getBoolDef def name req = fromMaybe def (getBoolOpt name req)

getDoubleDef :: Double -> ParamName -> Request -> Double
getDoubleDef def name req = fromMaybe def (getDoubleOpt name req)
