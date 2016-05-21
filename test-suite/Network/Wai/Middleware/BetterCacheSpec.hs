{-# LANGUAGE UnicodeSyntax #-}

module Network.Wai.Middleware.BetterCacheSpec where

import           Test.Hspec
import           Network.Wai.Middleware.BetterCache

spec ∷ Spec
spec = do
  describe "betterCache" $
    it "caches" $
      pending
