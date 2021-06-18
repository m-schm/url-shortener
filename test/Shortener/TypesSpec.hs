{-# LANGUAGE TypeApplications #-}
module Shortener.TypesSpec (spec) where

import Data.Either (isLeft, isRight)
import Data.Proxy (Proxy(..))
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import Data.Text.Lazy.Encoding (encodeUtf8)
import Servant (parseUrlPiece, mimeUnrender, PlainText)
import Shortener.TestOrphans ()
import Shortener.Types
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.QuickCheck.Instances ()

spec :: Spec
spec = do
  describe "ShortId" $
    prop "only parses with correct string length" $ \(ShortId str) ->
      let correctLength = T.length str == shortIdLen
          correctEitherTag = if correctLength then isRight else isLeft
          len = if correctLength then "correct length" else "incorrect length"
      in label len $ correctEitherTag (parseUrlPiece @ShortId str)

  describe "FullUrl" $ do
    it "fails on an empty string" $
      isLeft $ mimeUnrender @PlainText @FullUrl Proxy ""

    prop "succeeds with a protocol" $
      -- encode into ByteString from Text to ensure valid utf-8
      isRight . mimeUnrender @PlainText @FullUrl Proxy . ("foo://" <>) . encodeUtf8

    prop "fails without a protocol" $ \t ->
      not (LT.isInfixOf "://" t) && not (LT.null t)
        ==> isLeft (mimeUnrender @PlainText @FullUrl Proxy $ encodeUtf8 t)
