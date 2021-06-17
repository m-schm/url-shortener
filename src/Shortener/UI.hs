{-# LANGUAGE DataKinds, TypeOperators #-} -- for servant
module Shortener.UI (API, server) where

import Control.Monad.Error.Class (MonadError)
import Data.ByteString (ByteString)
import Data.Text.Encoding (encodeUtf8)
import Data.Void
import Servant
import qualified Shortener.API as Internal
import Shortener.Monad
import Shortener.Types
import Shortener.UI.AlwaysRedirect

type StaticPages = Get '[AlwaysRedirect] Void :<|> Raw

type API =
       "shorten" :> StaticPages
  :<|> Capture "id" ShortId :> Get '[AlwaysRedirect] Void
  :<|> "api" :> Internal.API
  :<|> StaticPages -- catchall; needs to be last

server :: (MonadShortener m, MonadError ServerError m) => ServerT API m
server = staticPage :<|> unshorten :<|> Internal.server :<|> staticPage

staticPage :: MonadError ServerError m => ServerT StaticPages m
staticPage = redirectTo "index.html" :<|> serveDirectoryWebApp "static/"

unshorten :: (MonadShortener m, MonadError ServerError m) => ShortId -> m Void
unshorten shortId = do
  fullUrl <- expand shortId
  case fullUrl of
    Just (FullUrl t) -> redirectTo (encodeUtf8 t)
    Nothing          -> redirectTo "/404.html"

redirectTo :: MonadError ServerError m => ByteString -> m a
redirectTo url = throwError $ err301
  { errHeaders = [("Location", url)]
  }
