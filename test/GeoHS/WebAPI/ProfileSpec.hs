{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module GeoHS.WebAPI.ProfileSpec (main, spec) where

import TestUtil
import GeoHS.WebAPI.Profile

import Data.Proxy
import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Instances()

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $ do
  describe "JSON instances" $ do
    validJSONProperty (Proxy :: Proxy Email)
    validJSONProperty (Proxy :: Proxy ProfilePublicData)
    validJSONProperty (Proxy :: Proxy Profile)

instance Arbitrary Profile where
  arbitrary = oneof [ profile, publicProfile ] 
    where
      profile = Profile <$> arbitrary <*> arbitrary <*> arbitrary
      publicProfile = PublicProfile <$> arbitrary <*> arbitrary

instance Arbitrary Email where
  arbitrary = Email <$> arbitrary

instance Arbitrary ProfilePublicData where
  arbitrary = ProfilePublicData <$> arbitrary
