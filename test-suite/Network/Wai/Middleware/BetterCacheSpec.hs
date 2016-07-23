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
        matchesKey (MockWrite Immutable _ k' _) = k == k'
        matchesKey (MockWrite (SMaxAge age) t' k' _) = k == k' && (fromIntegral $ t - t') < age
        matchesKey (MockWrite (MaxAge  age) t' k' _) = k == k' && (fromIntegral $ t - t') < age
        matchesKey (MockWrite _ _ _ _) = False
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
          hitCount ← liftIO $ newMVar 0
          curTime ← liftIO $ newMVar (0 ∷ CurrentTime)
          let app _ respond = do
                liftIO $ modifyMVar_ hitCount $ return . (+1)
                respond $ responseLBS status200 [("Cache-Control", "max-age=100")] "Hello World"
              conf = (cacheConf (MockBackend primEvs) (MockBackend secEvs)) { getCurrentTime = readMVar curTime }
              cachedApp = betterCache conf app
          return (primEvs, secEvs, hitCount, curTime, cachedApp)
        shouldBeCorrect rsp = do
          simpleStatus rsp `shouldBe` ok200
          simpleBody rsp `shouldBe` "Hello World"
        doGetReq cachedApp =
          runSession (request defaultRequest) cachedApp >>= shouldBeCorrect

    it "caches GET requests, serves them back for a given time" $ do
      (_, _, hitCount, curTime, cachedApp) ← things
      doGetReq cachedApp
      doGetReq cachedApp
      readMVar hitCount >>= (`shouldBe` 1)
      modifyMVar_ curTime $ return . (+10)
      doGetReq cachedApp
      readMVar hitCount >>= (`shouldBe` 1)
      modifyMVar_ curTime $ return . (+90)
      doGetReq cachedApp
      readMVar hitCount >>= (`shouldBe` 2)
