{-# LANGUAGE UnicodeSyntax, OverloadedStrings, FlexibleContexts, DeriveGeneric #-}

module Network.Wai.Middleware.BetterCache.Keys (
  KeyHeader (..)
, KeyHeaders (..)
, KeyQueryParams (..)
, KeyGenerator (..)
, keyGenerator
, generateKey
) where

import qualified Data.CaseInsensitive as CI
import qualified Data.HashSet as Set
import           Data.ByteString hiding (elem, filter, map)
import           Data.Attoparsec.ByteString.Char8
import           Data.ByteArray.Hash
import           Data.Word (Word64)
import           Data.Maybe (fromMaybe)
import           Data.Hashable (Hashable)
import           Control.Applicative
import           GHC.Generics (Generic)
import           Network.HTTP.Types.Header
import           Network.Wai

-- TODO: https://tools.ietf.org/html/draft-ietf-httpbis-key-01
--       data KeyElement = KeyHeader (CI.CI ByteString) | Param KeyElement ByteString | Substr KeyElement ByteString ...
--       and add it to Header

data KeyHeader = Accept | AcceptEncoding | AcceptLanguage | Other HeaderName
               deriving (Eq, Show, Generic)

instance Hashable KeyHeader

parseRequestHeader ∷ CI.CI ByteString → KeyHeader
parseRequestHeader "Accept" = Accept
parseRequestHeader "Accept-Encoding" = AcceptEncoding
parseRequestHeader "Accept-Language" = AcceptLanguage
parseRequestHeader x = Other x

showRequestHeader ∷ KeyHeader → CI.CI ByteString
showRequestHeader Accept = CI.mk "Accept"
showRequestHeader AcceptEncoding = CI.mk "Accept-Encoding"
showRequestHeader AcceptLanguage = CI.mk "Accept-Language"
showRequestHeader (Other x) = x

data KeyHeaders = IgnoreHeaders | FilteredHeaders (Set.HashSet KeyHeader) | AllHeaders
                deriving (Eq, Show)

data KeyQueryParams = IgnoreQuery | FilteredQuery [ByteString] | FullQuery
                    deriving (Eq, Show)

data KeyGenerator = KeyGenerator KeyHeaders KeyQueryParams
                  deriving (Eq, Show)


generateKey ∷ KeyGenerator → Request → Word64
generateKey (KeyGenerator khdrs kprms) req =
  hash (map (\(k, v) → (k, fromMaybe "" v)) (getParams kprms)
     ++ map (\(k, v) → (CI.foldedCase $ showRequestHeader k, v)) (getHeaders khdrs))
       (SipKey 123 456)
  where hash [] _ = 0
        hash ((k, v) : []) hashkey =
          let (SipHash hr) = sipHash hashkey $ k `append` v in hr
        hash ((k, v) : kvs) hashkey@(SipKey _ hk2) =
          let (SipHash hr) = sipHash hashkey $ k `append` v in hash kvs $ SipKey hk2 hr
        hdrs = map (\(k, v) → (parseRequestHeader k, v)) $ requestHeaders req
        getHeaders IgnoreHeaders = []
        getHeaders (FilteredHeaders hs) = filter (\(k, _) → k `elem` hs) hdrs
        getHeaders AllHeaders = hdrs
        query = queryString req
        getParams IgnoreQuery = []
        getParams (FilteredQuery ps) = filter (\(k, _) → k `elem` ps) query
        getParams FullQuery = query


keyGenerator ∷ Response → Either String KeyGenerator
keyGenerator resp = do
  let hdrs = responseHeaders resp
  keyhs   ← parseOnly keyHeaderParser   $ intercalate "," $ [ v | (k, v) ← hdrs, k == hVary || k == "Key" ]
  queryps ← parseOnly queryHeaderParser $ intercalate "," $ [ v | (k, v) ← hdrs, k == "Vary-Query" ]
  return $ KeyGenerator keyhs queryps

keyHeaderParser ∷ Parser KeyHeaders
keyHeaderParser = (skipSpace >> char '*' >> skipSpace >> return AllHeaders)
              <|> (header `sepBy1'` (char ',') >>= return . FilteredHeaders . Set.fromList)
              <|> return IgnoreHeaders
  where header = skipSpace >> (acceptEncoding <|> acceptLanguage <|> accept <|> other) <* skipSpace
        accept = stringCI "Accept" >> return Accept
        acceptEncoding = stringCI "Accept-Encoding" >> return AcceptEncoding
        acceptLanguage = stringCI "Accept-Language" >> return AcceptLanguage
        other = takeWhile1 (\x → x /= ',' && not (isSpace x)) >>= return . Other . CI.mk

queryHeaderParser ∷ Parser KeyQueryParams
queryHeaderParser = (skipSpace >> char '!' >> char '*' >> skipSpace >> return IgnoreQuery)
                <|> (query `sepBy1'` (char ',') >>= return . FilteredQuery)
                <|> return FullQuery
  where query = skipSpace >> takeWhile1 (\x → x /= ',' && not (isSpace x)) <* skipSpace
