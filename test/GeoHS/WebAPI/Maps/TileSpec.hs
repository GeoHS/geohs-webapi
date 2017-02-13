{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
module GeoHS.WebAPI.Maps.TileSpec (main, spec) where

import GeoHS.WebAPI

import Control.Lens hiding (Zoom, transform)

import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck
import Text.Printf (printf)

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $ do
  describe "fromTileIndexWith" $ do
    prop "behaves like fromTileIndex @SphericalMercator with tileMatrixSphericalMercator" $ \tix ->
      let fromTileIndex', fromTileIndex'' :: TileIndex -> Coords SphericalMercator
          fromTileIndex' = fromTileIndex
          fromTileIndex'' = fromTileIndexWith tileMatrixSphericalMercator
          res' = fromTileIndex' tix
          res'' = fromTileIndex'' tix
      in counterexample (printf "%s =/ %s" (show res') (show res'')) $
           res' `almostEqCoords` res''

  describe "toTileIndexWith" $ do
    prop "behaves like toTileIndex @SphericalMercator with tileMatrixSphericalMercator" $
      forAll ((,) <$> arbitraryZoom <*> arbitrary) $ \(z,cs) ->
      let toTileIndex', toTileIndex'' :: Zoom -> Coords SphericalMercator -> TileIndex
          toTileIndex' = toTileIndex
          toTileIndex'' = toTileIndexWith tileMatrixSphericalMercator
          res' = toTileIndex' z cs
          res'' = toTileIndex'' z cs
      in counterexample (printf "%s =/ %s" (show res') (show res'')) $ res' == res''

almostEqCoords :: Coords crs -> Coords crs -> Bool
almostEqCoords (Coords a b) (Coords c d) = abs (a-c) <= 1e-6 && abs (b-d) <= 1e-6

instance Arbitrary TileIndex where
  arbitrary = do
    z <- arbitraryZoom
    x <- arbitraryTileRowOrColForZoom z
    y <- arbitraryTileRowOrColForZoom z
    pure TileIndex {z,x,y}

instance Arbitrary (Coords SphericalMercator) where
  arbitrary = Coords <$> choose (w,e) <*> choose (s,n) where
    BoundingBox { boundingBoxEastSouth = Coords e s
                , boundingBoxWestNorth = Coords w n
                } = tileMatrixSphericalMercator^.extent


arbitraryZoom :: Gen Zoom
arbitraryZoom = choose (0, 20)

arbitraryTileRowOrColForZoom :: Zoom -> Gen Int
arbitraryTileRowOrColForZoom z = choose (0, 2^z)
