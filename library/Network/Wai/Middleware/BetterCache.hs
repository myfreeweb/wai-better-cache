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

import           Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import           Data.Attoparsec.ByteString.Char8 as AP
import           Data.IORef
import           Data.Maybe (fromMaybe)
import           Data.Word (Word64)
import           GHC.Int (Int64)
import           Control.Applicative
import           Control.Monad (liftM)
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
                     deriving (Eq, Show)

class CacheBackend α κ ω where
  cacheRead ∷ α → CurrentTime → κ → IO (Maybe ω)
  cacheWrite ∷ α → CacheExpiration → CurrentTime → κ → ω → IO ()
  cacheInvalidate ∷ α → κ → IO ()


data CachedRequest = CachedRequest Status ResponseHeaders BL.ByteString
                   deriving (Eq, Show)

data CacheControl = Unknown | NoStore | Immutable | MaxAge Int | SMaxAge Int
                  deriving (Eq, Show)

instance Monoid CacheControl where
  mempty = Unknown
  Unknown `mappend` x = x
  x `mappend` Unknown = x
  NoStore `mappend` _ = NoStore
  _ `mappend` NoStore = NoStore
  Immutable `mappend` _ = Immutable
  _ `mappend` Immutable = Immutable
  MaxAge _ `mappend` SMaxAge x = SMaxAge x
  SMaxAge x `mappend` MaxAge _ = SMaxAge x
  MaxAge _ `mappend` MaxAge b = MaxAge b
  SMaxAge _ `mappend` SMaxAge b = SMaxAge b

type PrimaryCacheKey = ByteString

type SecondaryCacheKey = Word64

data CacheConf where
  CacheConf ∷ (CacheBackend π PrimaryCacheKey KeyGenerator,
               CacheBackend σ SecondaryCacheKey CachedRequest) ⇒
    { primaryBackend ∷ π
    , secondaryBackend ∷ σ
    , getCurrentTime ∷ IO CurrentTime
    , primaryRequestKey ∷ Request → PrimaryCacheKey
    , secondaryRequestKey ∷ KeyGenerator → Request → SecondaryCacheKey
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
defaultPrimaryRequestKey req = (fromMaybe "" $ requestHeaderHost req)
                      `append` (BS.takeWhile (/= 63) $ rawPathInfo req)

defaultSecondaryRequestKey ∷ KeyGenerator → Request → SecondaryCacheKey
defaultSecondaryRequestKey = generateKey

defaultCacheControl ∷ Request → Response → CacheControl
defaultCacheControl _ resp = either (const NoStore) id $ parseOnly cacheControlHeaderParser ctrlhdr
  where ctrlhdr = intercalate "," $ [ v | (k, v) ← hdrs, k == hCacheControl ]
        hdrs = responseHeaders resp

cacheControlHeaderParser ∷ Parser CacheControl
cacheControlHeaderParser = liftM mconcat $ directive `sepBy1'` (char ',')
  where directive = skipSpace >> (noStore <|> immutable <|> maxAge <|> sMaxAge <|> other) <* skipSpace
        noStore = (stringCI "no-store" <|> stringCI "private") >> return NoStore
        immutable = stringCI "immutable" >> return Immutable
        maxAge = stringCI "max-age" >> num >>= return . MaxAge
        sMaxAge = stringCI "s-maxage" >> num >>= return . SMaxAge
        other = AP.takeWhile (\x → x /= ',') >> return Unknown
        num = skipSpace >> char '=' >> (decimal <|> (char '"' >> decimal <* char '"')) <* skipSpace

-- https://s3.amazonaws.com/haddock.stackage.org/nightly-2016-03-25/wai-middleware-caching-0.1.0.2/src/Network-Wai-Middleware-Cache.html
responseToLBS ∷ Response → IO BL.ByteString
responseToLBS resp =
  let (_, _, f) = responseToStream resp in
    f $ \body → do
      builderRef ← newIORef mempty
      body (\b → atomicModifyIORef builderRef $ \builder → (builder `mappend` b, ())) (return ())
      toLazyByteString <$> readIORef builderRef
