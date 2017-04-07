{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
module GeoHS.WebAPI.Maps.LayerSpec (main, spec) where

import GeoHS.WebAPI

import Control.Lens
import Data.Aeson
import Data.Monoid
import Data.Swagger
import Data.Swagger.Schema.Validation (validateToJSON)
import Data.Proxy
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck
import Data.Typeable
import Test.QuickCheck.Instances()
import SpatialReference

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $ do
  describe "JSON instances" $ do
    validJSONProperty (Proxy :: Proxy MapnikStyle)
    validJSONProperty (Proxy :: Proxy Source)
    validJSONProperty (Proxy :: Proxy Layer)

validJSONProperty
  :: forall p a. (Show a, Typeable a, ToSchema a, Arbitrary a, ToJSON a)
  => p a -> Spec
validJSONProperty _ = prop ("validateToJSON (" ++ show (typeOf (undefined :: a)) ++ ")") $
  \(ob :: a) -> let cexample = show (validateToJSON ob, encode ob)
                in counterexample cexample $ null (validateToJSON ob)

instance Arbitrary Layer where
  arbitrary = Layer
    <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
    <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
    <*> arbitrary <*> arbitrary

instance Arbitrary EPSG where
  arbitrary = maybe arbitrary pure =<<  mkEPSG . getPositive <$> arbitrary

instance Arbitrary LayerName where
  arbitrary = LayerName <$> arbitrary

instance Arbitrary WmsLayer where
  arbitrary = WmsLayer <$> arbitrary <*> arbitrary

instance Arbitrary Bounds where
  arbitrary = do
    sw <- LatLng <$> arbitrary <*> arbitrary
    w <- getPositive <$> arbitrary
    h <- getPositive <$> arbitrary
    pure $ emptyBounds
      & southWest .~ sw
      & width     .~ w
      & height    .~ h

instance Arbitrary Url where
  arbitrary = Url . ("http://"<>) <$> arbitrary

instance Arbitrary LatLng where
  arbitrary = LatLng <$> arbitrary <*> arbitrary

instance Arbitrary Attribution where
  arbitrary = Attribution <$> arbitrary <*> arbitrary

instance Arbitrary Source where
  arbitrary = oneof [ wmsSource, tileSource, mapnikSource ] 
    where
      wmsSource = WmsSource <$> arbitrary <*> arbitrary <*> arbitrary
      tileSource = TileSource <$> arbitrary <*> arbitrary
      mapnikSource = MapnikSource <$> arbitrary

instance Arbitrary MapnikStyle where
  arbitrary = oneof [ MapnikStylePath <$> arbitrary
                    , MapnikStyleString <$> arbitrary
                    ] 
