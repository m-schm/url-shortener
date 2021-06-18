module Shortener.Types
  ( ShortId(..), FullUrl(..), shortIdLen
  ) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Bifunctor (bimap)
import qualified Data.ByteString.Lazy.Char8 as LB8
import Data.Char (isAlpha)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8')
import Database.Persist.Sql (PersistField, PersistFieldSql)
import Servant

-- | Newtype wrapper around the shortened URL's ID - i.e. just the part after
-- the slash
newtype ShortId = ShortId Text
  deriving stock Show
  deriving newtype
    ( Eq
    , FromJSON, ToJSON
    , MimeRender PlainText, MimeUnrender PlainText
    , ToHttpApiData
    , PersistField, PersistFieldSql
    )

-- | Length (in characters) of generated 'ShortId's
--
-- @
-- shortIdLen = 6
-- @
shortIdLen :: Int
shortIdLen = 6

-- | Fails to parse unless the length is exactly 'shortIdLen'
instance FromHttpApiData ShortId where
  parseUrlPiece t
    | T.length t == shortIdLen = Right (ShortId t)
    | otherwise                = Left $ "Incorrect length: " <> t

-- | Newtype wrapper for full-size URLs
newtype FullUrl = FullUrl Text
  deriving stock Show
  deriving newtype
    ( Eq
    , FromJSON, ToJSON
    , MimeRender PlainText
    , FromHttpApiData, ToHttpApiData
    , PersistField, PersistFieldSql
    )

-- | Rejects if it doesn't contain a protocol, or if it's empty
instance MimeUnrender PlainText FullUrl where
  mimeUnrender _ ""  = Left "URL cannot be empty"
  mimeUnrender _ url =
    if LB8.isPrefixOf "://" (LB8.dropWhile isAlpha url)
    then bimap (const "Invalid UTF-8") FullUrl $ decodeUtf8' . LB8.toStrict $ url
    else Left "Protocol cannot be missing"
