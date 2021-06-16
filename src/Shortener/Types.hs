module Shortener.Types
  ( ShortId(..), FullUrl(..), shortIdLen
  ) where

import Servant
import Data.Text (Text)
import qualified Data.Text as T
import Data.Aeson (FromJSON, ToJSON)
import Database.Persist.Sql (PersistField, PersistFieldSql)

-- | Newtype wrapper around the shortened URL's ID - i.e. just the part after
-- the slash
newtype ShortId = ShortId Text
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

instance FromHttpApiData ShortId where
  parseUrlPiece t
    | T.length t == shortIdLen = Right (ShortId t)
    | otherwise                = Left $ "Incorrect length: " <> t

-- | Newtype wrapper for full-size URLs
newtype FullUrl = FullUrl Text
  deriving newtype
    ( Eq
    , FromJSON, ToJSON
    , MimeRender PlainText, MimeUnrender PlainText
    , FromHttpApiData, ToHttpApiData
    , PersistField, PersistFieldSql
    )
