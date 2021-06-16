module Shortener.UI (API, server) where

import Control.Monad.Error.Class (MonadError)
import Servant
import qualified Shortener.API as Internal
import Shortener.Monad
import Shortener.Types

type API =
       "shorten" :> Raw
  :<|> Capture "id" ShortId :> Raw
  :<|> "api" :> Internal.API
  :<|> Raw -- catchall; needs to be last

server :: (MonadShortener m, MonadError ServerError m) => ServerT API m
server = staticPage :<|> Tagged . unshorten :<|> Internal.server :<|> staticPage

staticPage :: ServerT Raw m
staticPage = serveDirectoryWebApp "static/"

unshorten :: ShortId -> Application
unshorten shortId req respond = do
  _
