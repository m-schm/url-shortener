module Shortener.DB
  ( PostgresHandler
  , runPostgresHandler
  ) where

import Control.Monad.Error.Class (MonadError)
import Control.Monad.Except (ExceptT(..), lift, runExceptT)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (ReaderT)
import Database.Persist
import Database.Persist.Postgresql
import Servant (Handler(..), ServerError)
import Shortener.API (FullUrl, ShortId)
import Shortener.Monad (MonadShortener(..))
import Shortener.Shorten (createShortIds)

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

instance MonadShortener (PostgresHandler backend) where
  shorten :: FullUrl -> PostgresHandler backend ShortId
  shorten fullUrl = do
    let urls = createShortIds fullUrl
    shortId <- shortenWith urls fullUrl
    writeUrl shortId fullUrl
    return shortId

  expand :: ShortId -> PostgresHandler backend (Maybe FullUrl)
  expand = error "TODO: expand"

shortenWith :: [ShortId] -> FullUrl -> PostgresHandler backend ShortId
shortenWith = _

writeUrl :: ShortId -> FullUrl -> PostgresHandler backend ()
writeUrl = _
