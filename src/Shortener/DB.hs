module Shortener.DB
  ( PostgresHandler(..)
  ) where

import Control.Monad.Error.Class (MonadError)
import Control.Monad.IO.Class (MonadIO(..))
import Servant (Handler, ServerError)
import Shortener.Monad (MonadShortener(..))
import Shortener.API (FullUrl, ShortId)
import Control.Monad.Reader (ReaderT)
import Database.Persist
import Control.Monad.Except (ExceptT)

data DbEnv = DbEnv {} -- TODO

runPostgresHandler :: PostgresHandler a -> Handler a
runPostgresHandler = _

newtype PostgresHandler a = PostgresHandler (Handler a)
  deriving newtype
    ( Functor, Applicative, Monad
    , MonadError ServerError
    )

instance MonadShortener PostgresHandler where
  shorten = error "TODO: shorten"
  expand = error "TODO: expand"
