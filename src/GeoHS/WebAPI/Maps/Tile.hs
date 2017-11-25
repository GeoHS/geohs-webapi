{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TemplateHaskell #-}
module GeoHS.WebAPI.Maps.Tile where

import GeoHS.Geometry
import Data.Aeson
import GHC.Generics (Generic)
import Mapnik
import Data.ByteString.Char8 (ByteString)
import Data.Swagger
import qualified Data.Text.Encoding as T
import qualified Data.ByteString.Base64 as B64
import Data.Bits (bit)
import Control.Lens hiding (Zoom, transform)

type Zoom = Int

-- http://wiki.openstreetmap.org/wiki/Slippy_map_tilenames
data TileIndex = TileIndex {z :: !Zoom, x :: !Int, y :: !Int }
  deriving (Eq, Ord, Show, ToJSON, FromJSON, Generic, ToSchema)

-- TODO: Move to hs-mapnik?
deriving instance ToSchema Box
deriving instance ToSchema ImageRgba8

data Tile = Tile
  { tileIndex  :: Maybe TileIndex
  , tileSrs    :: Proj4
  , tileExtent :: Box
  , tileImage  :: ImageRgba8
  } deriving (Generic, ToJSON, FromJSON, ToSchema)
makeFields ''Tile

instance HasWidth  Tile Int where width  = size._1
instance HasHeight Tile Int where height = size._2

data TileMatrix = TileMatrix
  { tileMatrixTileWidth  :: !Int
  , tileMatrixTileHeight :: !Int
  , tileMatrixExtent     :: !Box
  }
  deriving Show
makeFields ''TileMatrix

instance HasExtent (Tagged crs TileMatrix) (Tagged crs Box) where
  extent = taggedLens' extent
  {-# INLINE extent #-}

tileMatrix :: Int -> TileMatrix
tileMatrix sz = TileMatrix sz sz (Box 0 0 0 0)


class HasTileIndex crs where
  fromTileIndex :: TileIndex -> Tagged crs LatLng
  toTileIndex :: Zoom -> Tagged crs LatLng -> TileIndex


bboxSphericalMercator :: Tagged SphericalMercator Bounds
bboxSphericalMercator = (Tagged emptyBounds)
  & northWest .~ fromTileIndex (TileIndex 0 0 0)
  & southEast .~ fromTileIndex (TileIndex 0 1 1)

tileMatrixSphericalMercator :: TileMatrix
tileMatrixSphericalMercator = TileMatrix
  { tileMatrixTileWidth = 256
  , tileMatrixTileHeight = 256
  , tileMatrixBounds = unTagged bboxSphericalMercator
  }

instance HasTileIndex WGS84 where
  fromTileIndex TileIndex{z,x,y} = Tagged $ lngLat _lng _lat
    where
      _lng  = fromIntegral x / pow2 z * 360 - 180
      _lat  = 180 / pi * atan(0.5 * (exp n - exp (-n)))
      n    = pi - 2 * pi * fromIntegral y / pow2 z

  toTileIndex z cs = TileIndex{z,x,y}
    where
      x = truncate x'
      y = truncate y'
      x' = (cs^.lng + 180) / 360 * pow2 z
      y' = (1 - log (tan (cs^.lat * pi/180) + 1 / cos(cs^.lat * pi/180)) / pi)
         / 2 * pow2 z


maxResolution :: TileMatrix -> Double
maxResolution tm =
  max ((tm^.bounds.width) / (tm^.tileWidth.to fromIntegral))
      ((tm^.bounds.height) / (tm^.tileHeight.to fromIntegral))

resolutionForZoom :: TileMatrix -> Zoom -> Double
resolutionForZoom  tm z = maxResolution tm / pow2 z

resolutions :: TileMatrix -> [Double]
resolutions tm = map (resolutionForZoom tm) [0..]

fromTileIndexWith :: TileMatrix -> TileIndex -> LatLng
fromTileIndexWith tm TileIndex{z,x,y} = lngLat col row
  where
    col = tm^.bounds.west + (fromIntegral (x * tm^.tileWidth) * res)
    row = tm^.bounds.north + (fromIntegral (y * tm^.tileHeight) * (-res))
    res = resolutionForZoom tm z

data ReverseIntersectionPolicy = LowerTile | HigherTile

toTileIndexWithPolicy :: ReverseIntersectionPolicy -> TileMatrix -> Zoom -> LatLng -> TileIndex
toTileIndexWithPolicy pol tm z cs = TileIndex {z,x,y}
  where
    x = case pol of
      LowerTile -> ceiling x' - 1
      HigherTile -> floor x'
    y = case pol of
      LowerTile -> ceiling y' - 1
      HigherTile -> floor y'

    x' = fromIntegral x0' / fromIntegral (tm^.tileWidth) :: Double
    y' = fromIntegral y0' / fromIntegral (tm^.tileHeight) :: Double

    x0' = floor ( (cs^.lng - x0) / resolution + adjust ) :: Int
    y0' = floor ( (y0   - cs^.lat) / resolution - adjust ) :: Int

    resolution = resolutionForZoom tm z

    y0 = tm^.bounds.north
    x0 = tm^.bounds.west

    adjust = case pol of
      LowerTile -> 0.5
      HigherTile -> 0


toTileIndexWith :: TileMatrix -> Zoom -> LatLng -> TileIndex
toTileIndexWith = toTileIndexWithPolicy HigherTile

instance HasTileIndex SphericalMercator where
  fromTileIndex = transform  . (fromTileIndex :: TileIndex -> Tagged WGS84 LatLng)
  toTileIndex z = toTileIndex z   . (transform :: Tagged SphericalMercator LatLng-> Tagged WGS84 LatLng)

pow2 :: Int -> Double
pow2 = fromIntegral . (bit :: Int -> Int)



tileIndexBounds :: HasTileIndex crs => TileIndex -> Tagged crs Bounds
tileIndexBounds t@TileIndex{z,x,y} = Tagged emptyBounds
  & northWest .~ fromTileIndex t
  & southEast .~ fromTileIndex (TileIndex z (x+1) (y+1))

tileIndexBoundsWith :: TileMatrix -> TileIndex -> Bounds
tileIndexBoundsWith tm t@TileIndex{z,x,y} = emptyBounds
  & northWest .~ fromTileIndexWith tm t
  & southEast .~ fromTileIndexWith tm (TileIndex z (x+1) (y+1))
