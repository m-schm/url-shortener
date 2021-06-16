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
  backend -> PostgresHandler a -> Handler a
runPostgresHandler backend (PostgresHandler ma) =
  Handler . ExceptT . flip runSqlConn backend . lift . pure $ ma

newtype PostgresHandler a = PostgresHandler (Either ServerError a)
  deriving newtype
    ( Functor, Applicative, Monad
    , MonadError ServerError
    )

instance MonadShortener PostgresHandler where
  shorten = error "TODO: shorten"
  expand = error "TODO: expand"
