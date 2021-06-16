module Shortener.Shorten (createShortIds) where

import Crypto.Hash (Digest, SHA256)
import qualified Crypto.Hash as C
import Data.ByteArray (unpack)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.List (tails, foldl', transpose)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Data.Word (Word8)
import Shortener.API

-- | Creates an infinite list of 'ShortId's from a 'FullUrl' from its hash.
-- Deterministic shortening ensures that when shortening the same URL twice,
-- a pre-used row in the DB can be returned instead of creating multiple
-- unnecessary duplicates
createShortIds :: FullUrl -> [ShortId]
createShortIds (FullUrl t) =
  let hash :: Digest SHA256
      hash = C.hash (encodeUtf8 t)

      options :: [Integer]
      options = fmap fromBase256 $ tails (unpack hash)

  in fmap (iterate (1 +)) options
     &   concat . transpose
     <&> ShortId . T.pack . fmap (base64 !!) . toBase64

-- | Interprets a list of 'Word8's as an integer in base-256, with least
-- sigificant digit on the right
fromBase256 :: [Word8] -> Integer
fromBase256 = foldl' (\acc digit -> acc*256 + fromIntegral digit) 0

-- | Converts an 'Integer' to a list of base-64 digits, with least significant
-- digit on the right
toBase64 :: Integer -> [Int]
toBase64 = go [] where
  go acc 0 = acc
  go acc n =
    let (d, m) = n `divMod` 64
    in go (fromIntegral m : acc) d

-- | Youtube's web-safe base64 alphabet
base64 :: [Char]
base64 = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_"
