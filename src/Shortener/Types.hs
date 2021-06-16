module Shortener.Types
  ( ShortId(..), FullUrl(..)
  ) where

import Servant
import Data.Text (Text)
import Data.Aeson (FromJSON, ToJSON)
import Database.Persist.Sql (PersistField, PersistFieldSql)

-- | Newtype wrapper around the shortened URL's ID - i.e. just the part after
-- the slash
newtype ShortId = ShortId Text
  deriving newtype
    ( Eq
    , FromJSON, ToJSON
    , MimeRender PlainText, MimeUnrender PlainText
    , FromHttpApiData, ToHttpApiData
    , PersistField, PersistFieldSql
    )

-- | Newtype wrapper for full-size URLs
newtype FullUrl = FullUrl Text
  deriving newtype
    ( Eq
    , FromJSON, ToJSON
    , MimeRender PlainText, MimeUnrender PlainText
    , FromHttpApiData, ToHttpApiData
    , PersistField, PersistFieldSql
    )
