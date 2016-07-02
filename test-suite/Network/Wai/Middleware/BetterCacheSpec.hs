{-# LANGUAGE UnicodeSyntax, OverloadedStrings, MultiParamTypeClasses, FlexibleInstances #-}

module Network.Wai.Middleware.BetterCacheSpec where

import           Test.Hspec (Spec, describe, it)
import           Test.Hspec.Expectations.Pretty
import           Network.HTTP.Types.Status
import           Network.HTTP.Types.Header
import           Network.Wai
import           Network.Wai.Test
import           Network.Wai.Middleware.BetterCache
import           Control.Concurrent.MVar
import           Control.Monad.IO.Class
import           Debug.Trace
import           Safe

data MockEvent κ ω =   MockRead CurrentTime κ (Maybe ω)
                     | MockWrite CacheControl CurrentTime κ ω
                     | MockInvalidate κ
                     deriving (Show)

data MockBackend κ ω = MockBackend (MVar [MockEvent κ ω])

instance Eq κ ⇒ CacheBackend (MockBackend κ ω) κ ω where
  cacheRead (MockBackend esv) t k = do
    es ← takeMVar esv
    let matchesKey (MockRead _ _ _) = False
        matchesKey (MockWrite _ _ k' _) = k == k' -- TODO: Time check
        matchesKey (MockInvalidate k') = k == k'
        extractVal (MockRead _ _ v) = v
        extractVal (MockWrite _ _ _ v) = Just v
        extractVal (MockInvalidate _) = Nothing
        result = extractVal =<< headMay (filter matchesKey es)
    putMVar esv $ (MockRead t k result) : es
    return result
  cacheWrite (MockBackend esv) c t k v =
    modifyMVar_ esv $ \es → return $ (MockWrite c t k v) : es
  cacheInvalidate (MockBackend esv) k =
    modifyMVar_ esv $ \es → return $ (MockInvalidate k) : es

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

  describe "betterCache" $ do
    let things = do
          primEvs ← liftIO $ newMVar ([] ∷ [MockEvent PrimaryCacheKey KeyGenerator])
          secEvs ← liftIO $ newMVar ([] ∷ [MockEvent SecondaryCacheKey CachedResponse])
          let app req respond = respond $ responseLBS status200 [("Cache-Control", "max-age=100")] "Hello World"
              cachedApp = betterCache (cacheConf (MockBackend primEvs) (MockBackend secEvs)) app
          return (primEvs, secEvs, cachedApp)
    it "caches GET requests with the right headers" $ do
      (primEvs, secEvs, cachedApp) ← things
      rsp ← runSession (request defaultRequest) cachedApp
      traceShowM =<< readMVar primEvs
      traceShowM =<< readMVar secEvs
      simpleStatus rsp `shouldBe` ok200
