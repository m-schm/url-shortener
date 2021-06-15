module Shortener.DB
  ( PostgresHandler(..)
  ) where

import Control.Monad.Error.Class (MonadError)
import Control.Monad.IO.Class (MonadIO)
import Servant (Handler, ServerError)
import Shortener.Monad (MonadShortener(..))

newtype PostgresHandler a =
  PostgresHandler { runPostgresHandler :: Handler a }
  deriving newtype
    ( Functor, Applicative, Monad
    , MonadError ServerError, MonadIO )

instance MonadShortener PostgresHandler where
  shorten = error "TODO: shorten"
  expand = error "TODO: expand"
