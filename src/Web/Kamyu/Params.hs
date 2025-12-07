{-# LANGUAGE OverloadedStrings #-}

module Web.Kamyu.Params
    ( -- Basic parameter getters
      int, text, bool, double
    , intOpt, textOpt, boolOpt, doubleOpt
    , intDef, textDef, boolDef, doubleDef
    
    -- Types
    , ParamError(..)
    , defaultTo
    ) where

import Network.Wai (Request, queryString)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Read as TR
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Maybe (fromMaybe)

-- For user convenience, we accept String but work with Text internally
type ParamName = String

data ParamError = 
    MissingParam ParamName
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
-- PUBLIC API (all functions accept String for parameter names)
----------------------------------------------------------------

-- | Get required integer parameter
int :: ParamName -> Request -> Either ParamError Int
int name req = do
    text <- maybe (Left $ MissingParam name) Right (rawParam name req)
    case TR.decimal text of
        Right (val, "") -> Right val
        _ -> Left (InvalidInt name)

-- | Get required text parameter (returns String for user convenience)
text :: ParamName -> Request -> Either ParamError String
text name req =
    case rawParam name req of
        Just val -> Right (T.unpack val)
        Nothing -> Left (MissingParam name)

-- | Get required boolean parameter
bool :: ParamName -> Request -> Either ParamError Bool
bool name req = do
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
double :: ParamName -> Request -> Either ParamError Double
double name req = do
    textVal <- maybe (Left $ MissingParam name) Right (rawParam name req)
    case TR.double textVal of
        Right (val, "") -> Right val
        _ -> Left (InvalidDouble name)

----------------------------------------------------------------
-- OPTIONAL PARAMETERS
----------------------------------------------------------------

-- | Get optional integer parameter
intOpt :: ParamName -> Request -> Maybe Int
intOpt name req = do
    textVal <- rawParam name req
    case TR.decimal textVal of
        Right (val, "") -> Just val
        _ -> Nothing

-- | Get optional text parameter (returns String)
textOpt :: ParamName -> Request -> Maybe String
textOpt name req = do
    textVal <- rawParam name req
    Just (T.unpack textVal)

-- | Get optional boolean parameter
boolOpt :: ParamName -> Request -> Maybe Bool
boolOpt name req = do
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
doubleOpt :: ParamName -> Request -> Maybe Double
doubleOpt name req = do
    textVal <- rawParam name req
    case TR.double textVal of
        Right (val, "") -> Just val
        _ -> Nothing

----------------------------------------------------------------
-- PARAMETERS WITH DEFAULTS (most user-friendly)
----------------------------------------------------------------

-- | Get integer parameter with default value
intDef :: Int -> ParamName -> Request -> Int
intDef def name req = fromMaybe def (intOpt name req)

-- | Get text parameter with default value (returns String)
textDef :: String -> ParamName -> Request -> String
textDef def name req = fromMaybe def (textOpt name req)

-- | Get boolean parameter with default value
boolDef :: Bool -> ParamName -> Request -> Bool
boolDef def name req = fromMaybe def (boolOpt name req)

-- | Get double parameter with default value
doubleDef :: Double -> ParamName -> Request -> Double
doubleDef def name req = fromMaybe def (doubleOpt name req)

----------------------------------------------------------------
-- ERROR HANDLING HELPER
----------------------------------------------------------------

-- | Provide default value for Either result
defaultTo :: Either ParamError a -> a -> a
defaultTo (Right val) _ = val
defaultTo (Left _) def = def