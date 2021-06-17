module Shortener.DB
  ( PostgresHandler
  , runPostgresHandler
  ) where

import Control.Monad (void)
import Control.Monad.Error.Class (MonadError)
import Control.Monad.Except (ExceptT(..), runExceptT)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (ReaderT)
import Control.Monad.Trans (lift)
import Data.Functor ((<&>))
import Database.Persist
import Database.Persist.Postgresql
import GHC.Stack (HasCallStack)
import Servant (Handler(..), ServerError)
import Shortener.Types (FullUrl, ShortId)
import Shortener.DB.Schema
import Shortener.Monad (MonadShortener(..))
import Shortener.Shorten (createShortIds)

runPostgresHandler :: SqlBackend -> PostgresHandler a -> Handler a
runPostgresHandler backend (PostgresHandler ma) =
  Handler . ExceptT . flip runSqlConn backend . runExceptT $ ma

newtype PostgresHandler a =
  PostgresHandler (ExceptT ServerError (ReaderT SqlBackend IO) a)
  deriving newtype
    ( Functor, Applicative, Monad
    , MonadError ServerError, MonadIO
    )

-- | Is the 'ShortId' already in the database?
data ClaimedState
  = Unclaimed -- ^ A new entry, needs to be written
  | Duplicate -- ^ Already-seen entry, no need to be written

instance MonadShortener PostgresHandler where
  shorten :: FullUrl -> PostgresHandler ShortId
  shorten fullUrl = do
    let urls = createShortIds fullUrl
    (shortId, state) <- findUnclaimed fullUrl urls
    case state of
      Unclaimed -> writeUrl shortId fullUrl
      Duplicate -> pure () -- already in db, no need to write
    return shortId

  expand :: ShortId -> PostgresHandler (Maybe FullUrl)
  expand shortId = liftQuery (getBy $ UniqueShortId shortId)
    <&> fmap (urlFullUrl . entityVal)

-- | Finds a 'ShortId' from an infinite list that isn't yet used in the db;
-- returns it without writing anything. Fails on an empty list.
findUnclaimed :: HasCallStack =>
  FullUrl -> [ShortId] -> PostgresHandler (ShortId, ClaimedState)
findUnclaimed fullUrl = foldr tryThisOne bail where
  bail = error "findUnclaimed: reached end of infinite list"

  tryThisOne :: ShortId -> PostgresHandler (ShortId, ClaimedState)
    -> PostgresHandler (ShortId, ClaimedState)
  tryThisOne shortId nextTry = do
    found <- liftQuery $ getBy (UniqueShortId shortId)
    case found of
      Just (Entity _ v)
        | urlFullUrl v == fullUrl -> pure $ (shortId, Duplicate)
        | otherwise               -> nextTry
      Nothing                     -> pure $ (shortId, Unclaimed)

writeUrl :: ShortId -> FullUrl -> PostgresHandler ()
writeUrl shortId fullUrl = void $ liftQuery $ insert $ Url shortId fullUrl

liftQuery :: ReaderT SqlBackend IO a -> PostgresHandler a
liftQuery = PostgresHandler . lift
