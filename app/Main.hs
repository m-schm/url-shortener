module Main where

import Control.Monad.Logger (NoLoggingT(..))
import qualified Data.ByteString as BS
import Data.Proxy (Proxy(..))
import Database.Persist.Postgresql
  (withPostgresqlConn, runMigration, runSqlConn)
import qualified Network.Wai.Handler.Warp as Warp
import Servant (serve, hoistServer)
import Shortener.API (API)
import Shortener.DB (runPostgresHandler)
import Shortener.DB.Schema (migrateAll)
import Shortener.API (server)

main :: IO ()
main = do
  connStr <- BS.readFile "pgConnStr" -- TODO: move to actual config file format
  runNoLoggingT $ withPostgresqlConn connStr $ \sqlConn -> NoLoggingT $ do
    runSqlConn (runMigration migrateAll) sqlConn
    let p = Proxy :: Proxy API
    Warp.run 8080 $
      serve p $
        hoistServer p (runPostgresHandler sqlConn) server
