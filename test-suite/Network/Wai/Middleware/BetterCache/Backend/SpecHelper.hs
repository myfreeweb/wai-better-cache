{-# LANGUAGE UnicodeSyntax, OverloadedStrings, FlexibleContexts #-}

module Network.Wai.Middleware.BetterCache.Backend.SpecHelper where

import           Prelude hiding (read)
import           Data.ByteString as BS
import           Data.Typeable
import           Control.Monad.IO.Class (liftIO)
import           Test.Hspec (Spec, describe, it)
import           Test.Hspec.Expectations.Pretty
import           Network.Wai.Middleware.BetterCache

backendSpec ∷ (Typeable α, CacheBackend α ByteString ByteString) ⇒ α → Spec
backendSpec b = do
  describe (show $ typeOf b) $ do
    -- Apparently, GHC is not smart enough.
    -- It can't even determine that k and v will be ByteStrings from the constraint on backendSpec .
    -- That's what I get for writing OOP style Haskell :D
    let write m t k v = liftIO $ cacheWrite b m t (k ∷ ByteString) (v ∷ ByteString)
        read t k = liftIO (cacheRead b t (k ∷ ByteString) ∷ IO (Maybe ByteString))
        invalidate k = liftIO $ cacheInvalidate b (k ∷ ByteString) (Proxy ∷ Proxy ByteString)

    it "stores and returns values" $ do
      write (MaxAge 2) 0 "Key" "Val"
      read 1 "Key" >>= (`shouldBe` Just "Val")

    it "removes expired values on read" $ do
      write (MaxAge 2) 0 "Key" "Val"
      read 3 "Key" >>= (`shouldBe` Nothing)

    it "doesn't write uncacheable values" $ do
      write NoStore 0 "Nope" "NoVal"
      read 3 "Nope" >>= (`shouldBe` Nothing)

    it "invalidates values" $ do
      write (MaxAge 2) 0 "Key" "Val"
      invalidate "Key"
      read 1 "Key" >>= (`shouldBe` Nothing)
