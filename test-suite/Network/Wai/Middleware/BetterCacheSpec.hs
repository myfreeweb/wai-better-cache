{-# LANGUAGE UnicodeSyntax #-}

module Network.Wai.Middleware.BetterCacheSpec where

import           Test.Hspec (Spec, describe, it)
import           Test.Hspec.Expectations.Pretty
import           Network.HTTP.Types.Status
import           Network.HTTP.Types.Header
import           Network.Wai
import           Network.Wai.Middleware.BetterCache

spec ∷ Spec
spec = do
  let defreq = defaultRequest
  let cc = hCacheControl
  let ok = responseLBS ok200

  describe "defaultCacheControl" $ do
    let dCC = defaultCacheControl 
    it "parses Cache-Control" $ do
      dCC defreq (ok [] "") `shouldBe` Unknown
      dCC defreq (ok [(cc, "private")] "") `shouldBe` NoStore
      dCC defreq (ok [(cc, "max-age=123")] "") `shouldBe` MaxAge 123
      dCC defreq (ok [(cc, "s-maxage=321")] "") `shouldBe` SMaxAge 321
      dCC defreq (ok [(cc, "s-maxage=321, max-age=123")] "") `shouldBe` SMaxAge 321
      dCC defreq (ok [(cc, ", max-age=123,, \ts-maxage =\"321\" \t")] "") `shouldBe` SMaxAge 321
      dCC defreq (ok [(cc, ", max-age=123,, immutable, \ts-maxage =\"321\" \t")] "") `shouldBe` Immutable
      dCC defreq (ok [(cc, ",no-store, max-age=123,, immutable, \ts-maxage =\"321\" \t")] "") `shouldBe` NoStore

  describe "defaultPrimaryRequestKey" $ do
    let dPRK = defaultPrimaryRequestKey
    it "generates primary cache keys" $ do
      dPRK defreq `shouldBe` ""
      dPRK defreq { requestHeaderHost = Just "examp.le" } `shouldBe` "examp.le"
      dPRK defreq { requestHeaderHost = Just "examp.le", rawPathInfo = "/memes?type=dank" } `shouldBe` "examp.le/memes"
