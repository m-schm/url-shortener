module Shortener.API
  ( ShortId(..), FullUrl(..)
  , API
  ) where

import Servant
import Data.Text (Text)
import Data.Aeson (FromJSON, ToJSON)

-- | Newtype wrapper around the shortened URL's ID - i.e. just the part after
-- the slash
newtype ShortId = ShortId Text
  deriving newtype
    ( FromJSON, ToJSON
    , MimeRender PlainText, MimeUnrender PlainText
    , FromHttpApiData, ToHttpApiData
    )

-- | Newtype wrapper for full-size URLs
newtype FullUrl = FullUrl Text
  deriving newtype
    ( FromJSON, ToJSON
    , MimeRender PlainText, MimeUnrender PlainText
    , FromHttpApiData, ToHttpApiData
    )

type API =
       "shorten" :> ReqBody '[PlainText, JSON] FullUrl :> Post '[PlainText, JSON] ShortId
  :<|> Capture "id" ShortId :> Get '[PlainText, JSON] FullUrl
