module Main where

import Data.Proxy (Proxy(..))
import qualified Network.Wai.Handler.Warp as Warp
import Servant (serve, hoistServer)
import Shortener.API (API)
import Shortener.Server (server)
import Shortener.DB (runPostgresHandler)

main :: IO ()
main = Warp.run 8080 $
  serve @API Proxy $ hoistServer @API Proxy runPostgresHandler server
