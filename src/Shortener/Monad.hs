module Shortener.Monad where

import Shortener.API

-- | The 'MonadShortener' class represents being able to shorten a 'FullUrl' to
-- a 'ShortId' or (possibly) expand the other way. There is intentionally no
-- 'MonadIO' superclass, so it can be mocked in tests.
class Monad m => MonadShortener m where
  shorten :: FullUrl -> m ShortId
  expand :: ShortId -> m (Maybe FullUrl)
