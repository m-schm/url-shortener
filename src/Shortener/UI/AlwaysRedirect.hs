{-# LANGUAGE MultiParamTypeClasses #-}
module Shortener.UI.AlwaysRedirect where

import Servant
import Network.HTTP.Media ((//), (/:))
import Data.Void

-- | Dummy response type for HTML responses that always redirect
data AlwaysRedirect

instance Accept AlwaysRedirect where
  contentType _ = "text" // "html" /: ("charset", "utf-8")

instance MimeRender AlwaysRedirect Void where
  mimeRender _ = absurd
