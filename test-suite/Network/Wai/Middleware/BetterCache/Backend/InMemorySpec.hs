{-# LANGUAGE UnicodeSyntax, OverloadedStrings #-}

module Network.Wai.Middleware.BetterCache.Backend.InMemorySpec where

import           Network.Wai.Middleware.BetterCache.Backend.InMemory
import           Network.Wai.Middleware.BetterCache.Backend.SpecHelper
import           Data.ByteString as BS
import           Test.Hspec (Spec, runIO)

spec ∷ Spec
spec = backendSpec =<< runIO (newInMemoryCache Nothing ∷ IO (InMemoryCache ByteString ByteString))
