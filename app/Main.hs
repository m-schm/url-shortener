module Main where

import Control.Monad.Logger (NoLoggingT(..))
import qualified Data.ByteString as BS
import Data.Proxy (Proxy(..))
import Database.Persist.Postgresql (withPostgresqlConn)
import qualified Network.Wai.Handler.Warp as Warp
import Servant (serve, hoistServer)
import Shortener.API (API)
import Shortener.DB (runPostgresHandler)
import Shortener.Server (server)

main :: IO ()
main = do
  connStr <- BS.readFile "pgConnStr" -- TODO: move to actual config file format
  runNoLoggingT $ withPostgresqlConn connStr $ \sqlConn -> NoLoggingT $ do
    let p = Proxy @API
    Warp.run 8080 $
      serve p $
        hoistServer p (runPostgresHandler sqlConn) server
