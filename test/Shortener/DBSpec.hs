module Shortener.DBSpec (spec) where

import Control.Arrow ((>>>))
import Control.Monad.Except (runExceptT)
import qualified Data.ByteString as BS
import Database.Persist.Postgresql
import Database.PostgreSQL.Simple
import Servant (Handler(..))
import Shortener.DB
import Shortener.DB.Schema (migrateAll)
import Shortener.Monad
import Shortener.TestOrphans ()
import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Instances ()

spec :: Spec
spec
  = beforeAll setupDb
  $ afterAll teardownDb
  $ beforeWith (pure . fst)
  $ do
    it "shortening the same url twice gives the same code" $ \conn ->
      property $ \url ->
        testPg conn $ do
          a <- shorten url
          b <- shorten url
          pure (a === b)

    it "expanding then shortening gives the original" $ \conn ->
      property $ \url ->
        testPg conn $ do
          shortId <- shorten url
          fullUrl <- expand shortId
          pure (Just url === fullUrl)

testPg :: Testable a => SqlBackend -> PostgresHandler a -> Property
testPg backend = runPostgresHandler backend >>> runHandler' >>> runExceptT
  >>> fmap (either (property . const Discard) property)
  >>> ioProperty

setupDb :: IO (SqlBackend, Connection)
setupDb = do
  connStr <- BS.readFile "pgTestConnStr"
  conn <- connectPostgreSQL connStr
  backend <- openSimpleConn (\_ _ _ _ -> pure ()) conn
  runSqlConn (runMigration migrateAll) backend
  pure (backend, conn)

teardownDb :: (SqlBackend, Connection) -> IO ()
teardownDb (_, conn) = do
  -- HACK: I don't think Persistent lets you do this at all
  _ <- execute_ conn "DROP TABLE url;"
  close conn
