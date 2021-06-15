module Shortener.API
  ( ShortId(..), FullUrl(..)
  , API, server
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
    )

-- | Newtype wrapper for full-size URLs
newtype FullUrl = FullUrl Text
  deriving newtype
    ( FromJSON, ToJSON
    , MimeRender PlainText, MimeUnrender PlainText
    )

type API =
       "shorten" :> ReqBody '[PlainText, JSON] FullUrl :> Post '[PlainText, JSON] ShortId
  :<|> Capture "id" ShortId :> Get '[PlainText, JSON] FullUrl

server :: Server API
server = serveShorten :<|> serveUnshorten where

  serveShorten :: FullUrl -> Handler ShortId
  serveShorten fullUrl = error "TODO"

  serveUnshorten :: ShortId -> Handler FullUrl
  serveUnshorten shortUrl = error "TODO"
