{-# LANGUAGE UnicodeSyntax, OverloadedStrings, RecordWildCards, GADTs, MultiParamTypeClasses, FlexibleContexts, AllowAmbiguousTypes #-}

module Network.Wai.Middleware.BetterCache (
  betterCache

, CacheBackend (..)
, CurrentTime

, CacheConf (..)
, cacheConf
, CacheControl (..)
, defaultCacheControl
, PrimaryCacheKey
, defaultPrimaryRequestKey
, KeyGenerator
, SecondaryCacheKey
, defaultSecondaryRequestKey
, CachedResponse (..)
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

-- TODO: unsupported querystring params removal for servant

type CurrentTime = Int64

data CachedResponse = CachedResponse Status ResponseHeaders BL.ByteString
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

class CacheBackend α κ ω where
  cacheRead ∷ α → CurrentTime → κ → IO (Maybe ω)
  cacheWrite ∷ α → CacheControl → CurrentTime → κ → ω → IO ()
  cacheInvalidate ∷ α → κ → IO ()

type PrimaryCacheKey = ByteString

type SecondaryCacheKey = Word64

data CacheConf where
  CacheConf ∷ (CacheBackend π PrimaryCacheKey KeyGenerator,
               CacheBackend σ SecondaryCacheKey CachedResponse) ⇒
    { primaryBackend ∷ π
    , secondaryBackend ∷ σ
    , getCurrentTime ∷ IO CurrentTime
    , primaryRequestKey ∷ Request → PrimaryCacheKey
    , secondaryRequestKey ∷ KeyGenerator → Request → SecondaryCacheKey
    , cacheControl ∷ Request → Response → CacheControl
    } → CacheConf

cacheConf ∷ (CacheBackend π PrimaryCacheKey KeyGenerator,
             CacheBackend σ SecondaryCacheKey CachedResponse)
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

-- TODO: something about wai-extra gzip not adding Vary (vary by normalized encoding by default?)
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

betterCache ∷ CacheConf → Middleware
betterCache CacheConf{..} app req respond = do
  let primKey = primaryRequestKey req
  time ← getCurrentTime
  let goRequest = app req $ \rsp → do
        let cc = cacheControl req rsp
        case keyGenerator rsp of
             Left _ → respond rsp
             Right keyGen → do
               cacheWrite primaryBackend cc time primKey keyGen
               let secondKey = secondaryRequestKey keyGen req
               let status = responseStatus rsp
               let headers = responseHeaders rsp
               body ← responseToLBS rsp
               cacheWrite secondaryBackend cc time secondKey $ CachedResponse status headers body
               respond $ responseLBS status headers body
  keyGen' ← cacheRead primaryBackend time primKey ∷ IO (Maybe KeyGenerator)
  case keyGen' of
       Nothing → goRequest
       Just keyGen → do
         let secondKey = secondaryRequestKey keyGen req
         cachedRsp' ← cacheRead secondaryBackend time secondKey ∷ IO (Maybe CachedResponse)
         case cachedRsp' of
              Nothing → goRequest
              Just (CachedResponse s hs b) → respond $ responseLBS s hs b
