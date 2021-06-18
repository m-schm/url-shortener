{-# OPTIONS_GHC -Wno-orphans #-}
module Shortener.TestOrphans () where

import Control.Monad
import Data.Text (Text)
import qualified Data.Text as T
import Shortener.Types
import Shortener.Shorten (base64)
import Test.QuickCheck

instance Arbitrary FullUrl where
  arbitrary = do
    size <- getSize
    len <- subtract 1 <$> chooseBoundedIntegral (2, min 8 size)
    part <- word
    parts <- zipWith (flip T.snoc)
      <$> replicateM len sep
      <*> replicateM len word
    protocol <- word
    pure $ FullUrl $ protocol <> "://" <> part <> mconcat parts
    where
      word :: Gen Text
      word = T.pack <$> sized (flip replicateM alnum)

      alnum :: Gen Char
      alnum = oneof
        [ chooseEnum ('0', '9')
        , chooseEnum ('a', 'z')
        , chooseEnum ('A', 'Z')
        ]

      sep :: Gen Char
      sep = elements ['.', '/', '?', '&', '=']

instance Arbitrary ShortId where
  arbitrary = do
    len <- oneof [getSize, pure shortIdLen]
    ShortId . T.pack <$> replicateM len idChar
    where
      idChar = elements base64
