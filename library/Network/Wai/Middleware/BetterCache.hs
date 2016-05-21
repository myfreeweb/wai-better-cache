{-# LANGUAGE UnicodeSyntax, OverloadedStrings, GADTs, MultiParamTypeClasses, FlexibleContexts #-}

module Network.Wai.Middleware.BetterCache (
  CurrentTime
, CacheExpiration (..)
, CacheBackend (..)

, PrimaryCacheKey
, SecondaryCacheKey
, CachedRequest (..)
, CacheControl (..)
, CacheConf
, cacheConf
, defaultPrimaryRequestKey
, defaultSecondaryRequestKey
, defaultCacheControl
) where

import           Data.ByteString
import qualified Data.ByteString.Lazy as L
import           Data.IORef
import           GHC.Int (Int64)
import           System.Clock
import           Blaze.ByteString.Builder (toLazyByteString)
import           Network.HTTP.Types.Status
import           Network.HTTP.Types.Header
import           Network.Wai
import           Network.Wai.Middleware.BetterCache.Keys

-- TODO: something about wai-extra gzip not adding Vary (vary by normalized encoding by default?)
-- TODO: unsupported querystring params removal for servant

type CurrentTime = Int64
data CacheExpiration = Forever | Seconds Int
class CacheBackend α κ ω where
  cacheRead ∷ α → CurrentTime → κ → IO (Maybe ω)
  cacheWrite ∷ α → CacheExpiration → CurrentTime → κ → ω → IO ()
  cacheInvalidate ∷ α → κ → IO ()

type PrimaryCacheKey = ByteString
type SecondaryCacheKey = ByteString
data CachedRequest = CachedRequest Status ResponseHeaders L.ByteString
data CacheControl = Private | Immutable | MaxAge Int
data CacheConf where
  CacheConf ∷ (CacheBackend π PrimaryCacheKey KeyGenerator,
               CacheBackend σ SecondaryCacheKey CachedRequest) ⇒
    { primaryBackend ∷ π
    , secondaryBackend ∷ σ
    , getCurrentTime ∷ IO CurrentTime
    , primaryRequestKey ∷ Request → PrimaryCacheKey
    , secondaryRequestKey ∷ Request → KeyGenerator → SecondaryCacheKey
    , cacheControl ∷ Request → Response → CacheControl
    } → CacheConf

cacheConf ∷ (CacheBackend π PrimaryCacheKey KeyGenerator,
             CacheBackend σ SecondaryCacheKey CachedRequest)
          ⇒ π → σ → CacheConf
cacheConf pb sb =
  CacheConf { primaryBackend = pb
            , secondaryBackend = sb
            , getCurrentTime = sec <$> getTime Monotonic
            , primaryRequestKey = defaultPrimaryRequestKey
            , secondaryRequestKey = defaultSecondaryRequestKey
            , cacheControl = defaultCacheControl }

defaultPrimaryRequestKey ∷ Request → PrimaryCacheKey
defaultPrimaryRequestKey req = "TODO"

defaultSecondaryRequestKey ∷ Request → KeyGenerator → SecondaryCacheKey
defaultSecondaryRequestKey req keyhs = "TODO"

defaultCacheControl ∷ Request → Response → CacheControl
defaultCacheControl _ resp = Private

-- https://s3.amazonaws.com/haddock.stackage.org/nightly-2016-03-25/wai-middleware-caching-0.1.0.2/src/Network-Wai-Middleware-Cache.html
responseToLBS ∷ Response → IO L.ByteString
responseToLBS resp =
  let (_, _, f) = responseToStream resp in
    f $ \body → do
      builderRef ← newIORef mempty
      body (\b → atomicModifyIORef builderRef $ \builder → (builder `mappend` b, ())) (return ())
      toLazyByteString <$> readIORef builderRef
