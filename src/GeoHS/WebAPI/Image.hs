{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE KindSignatures #-}
module GeoHS.WebAPI.Image (
  module GeoHS.WebAPI.Image
, module GHC.TypeLits
) where

import GHC.TypeLits

data PNG8 (options :: Maybe Symbol)
data PNG (options :: Maybe Symbol)
data JPEG (quality :: Nat)
data WEBP (options :: Maybe Symbol)
