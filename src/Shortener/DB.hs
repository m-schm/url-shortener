module Shortener.DB
  ( PostgresHandler
  , runPostgresHandler
  ) where

import Control.Monad.Error.Class (MonadError)
import Servant (Handler(..), ServerError)
import Shortener.Monad (MonadShortener(..))
import Shortener.API (FullUrl, ShortId)
import Database.Persist
import Database.Persist.Postgresql
import Control.Monad.Except (ExceptT(..), lift)

runPostgresHandler :: BackendCompatible SqlBackend backend =>
  backend -> PostgresHandler backend a -> Handler a
runPostgresHandler backend (PostgresHandler ma) =
  Handler . ExceptT . flip runSqlConn backend . runExceptT $ ma

newtype PostgresHandler backend a =
  PostgresHandler (ExceptT ServerError (ReaderT backend IO) a)
  deriving newtype
    ( Functor, Applicative, Monad
    , MonadError ServerError, MonadIO
    )

instance MonadShortener PostgresHandler where
  shorten = error "TODO: shorten"
  expand = error "TODO: expand"
