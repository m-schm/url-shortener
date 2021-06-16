module Shortener.Shorten (shorten) where

import Shortener.API
import Data.Text.Encoding (encodeUtf8)
import Crypto.Hash (hashWith, SHA256(..))
import Data.ByteArray (unpack)

-- | Creates an infinite list of 'ShortId's from a 'FullUrl' from its hash.
-- Deterministic shortening ensures that when shortening the same URL twice,
-- a pre-used row in the DB can be returned instead of creating multiple
-- unnecessary duplicates
createShortIds :: FullUrl -> [ShortId]
createShortIds (FullUrl t) = _ $ unpack $ hashWith SHA256 $ encodeUtf8 t

-- | youtube's web-safe base64 alphabet
base64 :: [Char]
base64 = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_"
