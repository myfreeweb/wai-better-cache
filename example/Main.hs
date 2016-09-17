{-# LANGUAGE OverloadedStrings, UnicodeSyntax #-}

module Main where

import           Network.HTTP.Types
import           Network.Wai
import           Network.Wai.Middleware.BetterCache
import           Network.Wai.Middleware.BetterCache.Backend.InMemory
import           Network.Wai.Handler.Warp (run)
import           Control.Concurrent (threadDelay)
import           Data.Time.Clock.POSIX
import           Data.ByteString.Lazy.Char8 (pack)

app ∷ Application
app req respond = do
  ptime ← getPOSIXTime
  threadDelay 200000 -- microseconds
  respond $ case rawPathInfo req of
                 "/nocache" → responseLBS status200
                                          [ ("Content-Type", "text/plain")
                                          , ("Cache-Control", "no-store")] $
                                          pack $ show ptime
                 _ → responseLBS status200
                                 [ ("Content-Type", "text/plain")
                                 , ("Cache-Control", "public, max-age=2")] $
                                 pack $ show ptime

main ∷ IO ()
main = do
  putStrLn $ "http://localhost:8080"
  pbackend ← newInMemoryCache Nothing ∷ IO (InMemoryCache PrimaryCacheKey KeyGenerator)
  sbackend ← newInMemoryCache Nothing ∷ IO (InMemoryCache SecondaryCacheKey CachedResponse)
  let conf = cacheConf pbackend sbackend
  run 8080 $ betterCache conf app
