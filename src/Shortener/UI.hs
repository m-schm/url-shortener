module Shortener.UI (API) where

import Servant
import qualified Shortener.API as Internal
import Shortener.Types

type API =
       "shorten" :> Raw
  :<|> Raw
  :<|> Capture "id" ShortId :> Raw
  :<|> "api" :> Internal.API
