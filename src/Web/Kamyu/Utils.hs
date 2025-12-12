

module Web.Kamyu.Utils
    (
      toText
    , fromText
    , toString
    , Web.Kamyu.Utils.fromString
    , nonEmpty
    , safeRead
    ) where

import Data.Text (Text)
import Data.String (IsString(..))
import qualified Data.Text as T
import Text.Read (readMaybe)

-- | Convert any IsString type to Text
toText :: IsString s => s -> Text
toText s = T.pack (toString s)

-- | Convert Text to String
fromText :: Text -> String
fromText = T.unpack

-- | Convert any IsString type to String
toString :: IsString s => s -> String
toString s = T.unpack (toText s)

-- | Convert String to Text
fromString :: String -> Text
fromString = T.pack

-- | Check if string is not empty (useful for validation)
nonEmpty :: Text -> Bool
nonEmpty = not . T.null

-- | Safe read for integers (returns Maybe)
safeRead :: Text -> Maybe Int
safeRead = readMaybe . T.unpack