{-# LANGUAGE UnicodeSyntax, OverloadedStrings, FlexibleInstances, MultiParamTypeClasses, LambdaCase, DeriveDataTypeable #-}

module Network.Wai.Middleware.BetterCache.Backend.InMemory (
  InMemoryCache
, newInMemoryCache
) where

import           Data.Cache.LRU.IO as LRU
import           Data.Typeable
import           Network.Wai.Middleware.BetterCache

newInMemoryCache ∷ Ord κ ⇒ Maybe Integer → IO (InMemoryCache κ ω)
newInMemoryCache x = InMemoryCache <$> newAtomicLRU x

data InMemoryCache κ ω = InMemoryCache (AtomicLRU κ (CurrentTime, ω))
                         deriving Typeable

instance Ord κ ⇒ CacheBackend (InMemoryCache κ ω) κ ω where
  cacheRead (InMemoryCache lru) time k = LRU.lookup k lru >>= \case
    Just (exptime, v) | exptime > time → return $ Just v
    Just _ → delete k lru >> return Nothing
    Nothing → return Nothing
  cacheWrite (InMemoryCache lru) (SMaxAge maxage) time k v = cacheWrite (InMemoryCache lru) (MaxAge maxage) time k v
  cacheWrite (InMemoryCache lru) Immutable time k v = cacheWrite (InMemoryCache lru) (MaxAge 365000000) time k v
  cacheWrite (InMemoryCache lru) (MaxAge maxage) time k v = insert k (time + (fromIntegral maxage), v) lru
  cacheWrite _ _ _ _ _ = return ()
  cacheInvalidate (InMemoryCache lru) k _ = delete k lru >> return ()
