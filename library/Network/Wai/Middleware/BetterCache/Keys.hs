{-# LANGUAGE Trustworthy, UnicodeSyntax, OverloadedStrings, FlexibleInstances, FlexibleContexts, TupleSections, DeriveGeneric, TemplateHaskell #-}

module Network.Wai.Middleware.BetterCache.Keys (
  Filter (..)
, TakeHeaders (..)
, TakeQueryParams (..)
, KeyGenerator (..)
, keyGenerator
, generateKey
) where

import qualified Data.CaseInsensitive as CI
import           Data.ByteString hiding (elem, filter, map)
import           Data.Attoparsec.ByteString.Char8
import           Data.Word (Word64)
import           Data.Store.TH
import           Data.Maybe (fromMaybe, mapMaybe)
import           Control.Applicative
import           Control.Arrow (first, second)
import           GHC.Generics (Generic)
import           TH.Derive
import           Network.HTTP.Types.Status
import           Network.Wai.Middleware.BetterCache.SafeImports

-- TODO: https://tools.ietf.org/html/draft-ietf-httpbis-key-01
data Filter = KHeader (CI.CI ByteString) -- | Param Filter ByteString | Substr Filter ByteString ...
                deriving (Eq, Show, Generic)

applyElement ∷ RequestHeaders → Filter → Maybe Header
applyElement hdrs (KHeader hdrname) = (hdrname, ) <$> lookup hdrname hdrs

data TakeHeaders = IgnoreHeaders | FilteredHeaders [Filter] | AllHeaders
                deriving (Eq, Show, Generic)

data TakeQueryParams = IgnoreQuery | FilteredQuery [ByteString] | FullQuery
                    deriving (Eq, Show, Generic)

data KeyGenerator = KeyGenerator TakeHeaders TakeQueryParams
                  deriving (Eq, Show, Generic)

$($(derive [d|
        instance Deriving (Store Status)
        instance Deriving (Store (CI.CI ByteString))
        |]))
instance Store Filter
instance Store TakeHeaders
instance Store TakeQueryParams
instance Store KeyGenerator

generateKey ∷ KeyGenerator → Request → Word64
generateKey (KeyGenerator khdrs kprms) req =
  hash (   (map (second $ fromMaybe "") $ getParams kprms)
        ++ (map (first CI.foldedCase) $ getHeaders khdrs)
       ) (SipKey 123 456)
  where hash [] _ = 0
        hash ((k, v) : []) hashkey =
          let (SipHash hr) = sipHash hashkey $ k `append` v in hr
        hash ((k, v) : kvs) hashkey@(SipKey _ hk2) =
          let (SipHash hr) = sipHash hashkey $ k `append` v in hash kvs $ SipKey hk2 hr
        hdrs = requestHeaders req
        getHeaders IgnoreHeaders = []
        getHeaders (FilteredHeaders fltrs) = mapMaybe (applyElement hdrs) fltrs
        getHeaders AllHeaders = hdrs
        query = queryString req
        getParams IgnoreQuery = []
        getParams (FilteredQuery ps) = filter (\(k, _) → k `elem` ps) query
        getParams FullQuery = query


keyGenerator ∷ Response → Either String KeyGenerator
keyGenerator resp = do
  let hdrs = responseHeaders resp
  keyhs   ← parseOnly takeHeaderParser   $ intercalate "," $ [ v | (k, v) ← hdrs, k == hVary || k == "Key" ]
  queryps ← parseOnly takeQueryParser    $ intercalate "," $ [ v | (k, v) ← hdrs, k == "Vary-Query" ]
  return $ KeyGenerator keyhs queryps

takeHeaderParser ∷ Parser TakeHeaders
takeHeaderParser = (skipSpace >> char '*' >> skipSpace >> return AllHeaders)
              <|> (header `sepBy1'` (char ',') >>= return . FilteredHeaders . map KHeader) -- TODO Key parsing here
              <|> return IgnoreHeaders
  where header = skipSpace >> other <* skipSpace
        other = takeWhile1 (\x → x /= ',' && not (isSpace x)) >>= return . CI.mk

takeQueryParser ∷ Parser TakeQueryParams
takeQueryParser = (skipSpace >> char '!' >> char '*' >> skipSpace >> return IgnoreQuery)
                <|> (query `sepBy1'` (char ',') >>= return . FilteredQuery)
                <|> return FullQuery
  where query = skipSpace >> takeWhile1 (\x → x /= ',' && not (isSpace x)) <* skipSpace
