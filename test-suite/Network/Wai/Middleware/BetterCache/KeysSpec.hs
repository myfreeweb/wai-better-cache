{-# LANGUAGE UnicodeSyntax #-}

module Network.Wai.Middleware.BetterCache.KeysSpec where

import           Test.Hspec (Spec, describe, it)
import           Test.Hspec.Expectations.Pretty
import           Network.HTTP.Types.Status
import           Network.HTTP.Types.Header
import           Network.Wai
import           Network.Wai.Middleware.BetterCache.Keys

spec ∷ Spec
spec = do
  describe "keyGenerator" $
    it "parses Vary/Key/Vary-Query headers into a KeyGenerator" $ do
      keyGenerator (responseLBS ok200 [] "")
        `shouldBe` Right (KeyGenerator IgnoreHeaders FullQuery)
      keyGenerator (responseLBS ok200 [(hVary, "")] "")
        `shouldBe` Right (KeyGenerator IgnoreHeaders FullQuery)
      keyGenerator (responseLBS ok200 [(hVary, "*")] "")
        `shouldBe` Right (KeyGenerator AllHeaders FullQuery)
      keyGenerator (responseLBS ok200 [(hVary, "Accept-Encoding")] "")
        `shouldBe` Right (KeyGenerator (FilteredHeaders [ KHeader "Accept-Encoding" ]) FullQuery)
      keyGenerator (responseLBS ok200 [(hVary, "Accept,    Accept-Encoding"), ("kEY", " Cookie\t")] "")
        `shouldBe` Right (KeyGenerator (FilteredHeaders [ KHeader "Accept", KHeader "Accept-Encoding", KHeader "Cookie" ]) FullQuery)
      keyGenerator (responseLBS ok200 [(hVary, "Accept,    Accept-Encoding"), ("Vary-Query", " page, perPage\t"), ("kEY", " Cookie\t"), ("Vary-Query", "v")] "")
        `shouldBe` Right (KeyGenerator (FilteredHeaders [ KHeader "Accept", KHeader "Accept-Encoding", KHeader "Cookie" ])
                                       (FilteredQuery [ "page", "perPage", "v" ]))
      keyGenerator (responseLBS ok200 [("Vary-Query", " page, perPage\t"), ("Vary-Query", "v")] "")
        `shouldBe` Right (KeyGenerator IgnoreHeaders (FilteredQuery [ "page", "perPage", "v" ]))

  describe "generateKey" $
    it "generates keys" $ do
      generateKey (KeyGenerator IgnoreHeaders FullQuery)
                  defaultRequest { requestHeaders = [ (hAcceptEncoding, "gzip") ] }
        `shouldBe` 0
      generateKey (KeyGenerator (FilteredHeaders [ KHeader "Accept-Encoding" ]) FullQuery)
                  defaultRequest { requestHeaders = [ ("Whatever", "Test") ] }
        `shouldBe` 0
      generateKey (KeyGenerator (FilteredHeaders [ KHeader "Accept-Encoding" ]) FullQuery)
                  defaultRequest { requestHeaders = [ (hAcceptEncoding, "gzip"), (hAccept, "*") ] }
        `shouldBe` 7466701558882623328
      generateKey (KeyGenerator (FilteredHeaders [ KHeader "Accept-Encoding" ]) FullQuery)
                  defaultRequest { requestHeaders = [ (hAcceptEncoding, "gzip"), (hAccept, "text/html") ] }
        `shouldBe` 7466701558882623328
      generateKey (KeyGenerator AllHeaders FullQuery)
                  defaultRequest { requestHeaders = [ (hAcceptEncoding, "gzip"), (hAccept, "text/html") ] }
        `shouldBe` 698436548535444120
      generateKey (KeyGenerator AllHeaders FullQuery)
                  defaultRequest { requestHeaders = [ (hAcceptEncoding, "gzip"), (hAccept, "text/html") ]
                                 , queryString = [ ("a", Just "x"), ("b", Just "y") ] }
        `shouldBe` 10546968719036233826
      generateKey (KeyGenerator IgnoreHeaders (FilteredQuery [ "a" ]))
                  defaultRequest { queryString = [ ("a", Just "x"), ("b", Just "y") ] }
        `shouldBe` 12769698987370781088
      generateKey (KeyGenerator IgnoreHeaders (FilteredQuery [ "a" ]))
                  defaultRequest { queryString = [ ("a", Just "x"), ("b", Just "AAAAAAA") ] }
        `shouldBe` 12769698987370781088
      generateKey (KeyGenerator IgnoreHeaders (FilteredQuery [ "a", "b" ]))
                  defaultRequest { queryString = [ ("a", Just "x"), ("b", Just "y") ] }
        `shouldBe` 17001395044418906804
