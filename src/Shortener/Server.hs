module Shortener.Server (server) where

import Control.Monad.Error.Class
import qualified Data.Text.Encoding as T
import Servant
import Shortener.API
import Shortener.Monad
import qualified Data.ByteString.Lazy as LB

server :: ∀ m. (MonadShortener m, MonadError ServerError m) => ServerT API m
server = shorten :<|> unshorten where
  unshorten :: ShortId -> m FullUrl
  unshorten shortUrl = expand shortUrl
    >>= maybe (throwError $ notFound shortUrl) pure

notFound :: ShortId -> ServerError
notFound (ShortId sh) =
  let msg = "Not found: /" <> sh
  in err404 { errBody = LB.fromStrict $ T.encodeUtf8 msg }
