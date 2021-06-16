module Shortener.API (API, server) where

import Control.Monad.Error.Class
import qualified Data.Text.Encoding as T
import Servant
import Shortener.Types
import Shortener.Monad
import qualified Data.ByteString.Lazy as LB

type API =
       "shorten" :> ReqBody '[PlainText, JSON] FullUrl :> Put '[PlainText, JSON] ShortId
  :<|> Capture "id" ShortId :> Get '[PlainText, JSON] FullUrl

server :: ∀ m. (MonadShortener m, MonadError ServerError m) => ServerT API m
server = shorten :<|> unshorten where
  unshorten :: ShortId -> m FullUrl
  unshorten shortUrl = expand shortUrl
    >>= maybe (throwError $ notFound shortUrl) pure

notFound :: ShortId -> ServerError
notFound (ShortId sh) =
  let msg = "Not found: /" <> sh
  in err404 { errBody = LB.fromStrict $ T.encodeUtf8 msg }
