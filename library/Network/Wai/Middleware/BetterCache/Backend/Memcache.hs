{-# LANGUAGE Trustworthy, UnicodeSyntax, OverloadedStrings, FlexibleInstances, MultiParamTypeClasses, DeriveDataTypeable #-}

module Network.Wai.Middleware.BetterCache.Backend.Memcache (
  MemcacheCache
, newMemcacheCache
) where

import qualified Database.Memcache.Client as M
import qualified Database.Memcache.Types as M
import           Data.Typeable
import           Data.Store
import           Control.Monad (void)
import           Control.Error.Util (hush)
import           Network.Wai.Middleware.BetterCache

newMemcacheCache ∷ [M.ServerSpec] → M.Options → IO MemcacheCache
newMemcacheCache s o = MemcacheCache <$> M.newClient s o

data MemcacheCache = MemcacheCache M.Client
                     deriving Typeable

instance {-# OVERLAPPING #-} CacheBackend MemcacheCache M.Key M.Value where
  cacheRead (MemcacheCache c) _ k = return Nothing -- ((\(x, _, _) → x) <$>) <$> M.get c k
  -- memcache considers expiration times above 30 days to be unix timestamps
  -- which is absolutely terrible
  -- and we read the monotonic clock, not unix timestamps
  -- so whatever, 30 days max
  cacheWrite (MemcacheCache c) (SMaxAge maxage) _ k v = void $ M.set c k v (fromIntegral 0) (max (30 * 24 * 60 * 60) $ fromIntegral maxage)
  cacheWrite (MemcacheCache c) Immutable _ k v = void $ M.set c k v (fromIntegral 0) (fromIntegral 0)
  cacheWrite (MemcacheCache c) (MaxAge maxage) _ k v = void $ M.set c k v (fromIntegral 0) (max (30 * 24 * 60 * 60) $ fromIntegral maxage)
  cacheWrite _ _ _ _ _ = return ()
  cacheInvalidate (MemcacheCache c) k _ = void $ M.delete c k (fromIntegral 0)

instance {-# OVERLAPPABLE #-} (Store κ, Store ω) ⇒ CacheBackend MemcacheCache κ ω where
  cacheRead mc x k = do
    raw ← cacheRead mc x k
    return $ hush . decode =<< raw
  cacheWrite mc e t k v = cacheWrite mc e t (encode k) (encode v)
  cacheInvalidate mc k p = cacheInvalidate mc (encode k) p
