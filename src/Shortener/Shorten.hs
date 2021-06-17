module Shortener.Shorten (createShortIds) where

import Crypto.Hash (Digest, SHA256)
import qualified Crypto.Hash as C
import Data.ByteArray (unpack)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.List (foldl')
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Data.Word (Word8)
import Shortener.Types

-- | Creates an infinite list of 'ShortId's from a 'FullUrl' from its hash.
-- Deterministic shortening ensures that when shortening the same URL twice,
-- a pre-used row in the DB can be returned instead of creating multiple
-- unnecessary duplicates, while returning a stream of results ensures
-- 'ShortId's can still be generated in the face of hash collisions
--
-- __NOTE:__ While the list is indeed infinite, the current implementation will
-- repeat 'ShortId's after exhausting all possible length-'shortIdLen' strings.
-- This could be mitigated by bailing and switching to longer strings, but
-- seems like needless complexity for now
createShortIds :: FullUrl -> [ShortId]
createShortIds (FullUrl t) =
  let hash :: Digest SHA256
      hash = C.hash (encodeUtf8 t)

  in  fromBase256 (unpack hash)
  &   iterate (1 +)
  <&> ShortId . T.pack . take shortIdLen . fmap (base64 !!) . toBase64

-- | Interprets a list of 'Word8's as an integer in base-256, with least
-- sigificant digit on the right
fromBase256 :: [Word8] -> Integer
fromBase256 = foldl' (\acc digit -> acc*256 + fromIntegral digit) 0

-- | Converts an 'Integer' to a list of base-64 digits, with least significant
-- digit on the left
toBase64 :: Integer -> [Int]
toBase64 0 = []
toBase64 n =
  let (d, m) = n `divMod` 64
  in fromIntegral m : toBase64 d

-- | Youtube's web-safe base64 alphabet
--
-- @
-- base64 = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_"
-- @
base64 :: [Char]
base64 = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_"
