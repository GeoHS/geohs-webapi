{-# LANGUAGE DataKinds #-}
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
import GHC.Generics
import Data.ByteString.Char8 (ByteString)
import Data.Swagger
import qualified Data.Text.Encoding as T
import qualified Data.ByteString.Base64 as B64
import Data.Bits (bit)
import Control.Lens hiding (Zoom, transform)
import SpatialReference

type Zoom = Int

type WGS84 = Epsg 4326
type SphericalMercator = Epsg 3857

newtype RGBA8 = RGBA8 ByteString
  deriving (Eq, Ord, Show, Generic)


instance ToSchema RGBA8 where
  declareNamedSchema _ = return $ NamedSchema (Just "RGBA8") binarySchema

unRGBA :: RGBA8 -> ByteString
unRGBA (RGBA8 s) = s
{-# INLINE unRGBA #-}

instance ToJSON RGBA8 where
  toJSON = toJSON . T.decodeUtf8 . B64.encode . unRGBA

instance FromJSON RGBA8 where
  parseJSON = withText "RGBA8 must be a base64 encoded string" $
                either fail (pure . RGBA8) . B64.decode . T.encodeUtf8

-- http://wiki.openstreetmap.org/wiki/Slippy_map_tilenames
data TileIndex = TileIndex {z :: !Zoom, x :: !Int, y :: !Int }
  deriving (Eq, Ord, Show, ToJSON, FromJSON, Generic, ToSchema)

type Proj4String = String

data Tile = Tile
  { tileIndex    :: Maybe TileIndex
  , tileProj4    :: Proj4String
  , tileBounds   :: Bounds
  , tileSize     :: (Int,Int)
  , tileRgbaData :: RGBA8
  } deriving (Generic, ToJSON, FromJSON, ToSchema)
makeFields ''Tile

instance HasWidth Tile Int where
  width = size._1

instance HasHeight Tile Int where
  height = size._2


data BoundingBox crs =
  BoundingBox {
    boundingBoxWestNorth :: !(Coords crs)
  , boundingBoxEastSouth :: !(Coords crs)
  } deriving (Eq, Ord, Show)
makeFields ''BoundingBox

bbox :: BoundingBox crs
bbox = BoundingBox (Coords 0 0) (Coords 0 0)


instance HasNorth (BoundingBox crs) Double where north = westNorth.lat
instance HasSouth (BoundingBox crs) Double where south = eastSouth.lat
instance HasWest (BoundingBox crs) Double where west = westNorth.lng
instance HasEast (BoundingBox crs) Double where east = eastSouth.lng

data TileMatrix crs = TileMatrix
  { tileMatrixTileWidth  :: !Int
  , tileMatrixTileHeight :: !Int
  , tileMatrixExtent :: !(BoundingBox crs)
  }
  deriving Show
makeFields ''TileMatrix


tileMatrix :: Int -> TileMatrix crs
tileMatrix sz = TileMatrix sz sz bbox


class HasTileIndex crs where
  fromTileIndex :: TileIndex -> Coords crs
  toTileIndex :: Zoom -> Coords crs -> TileIndex

class HasTransform crsFrom crsTo where
  transform :: Coords crsFrom -> Coords crsTo



instance HasTransform WGS84 SphericalMercator where
  transform (Coords _lng _lat) = Coords lng1 lat1
    where
      lng1 = _lng * originShift / 180
      lat1 = (log (tan((90 + _lat) * pi / 360 )) / (pi / 180)) * originShift / 180

instance HasTransform SphericalMercator WGS84 where
  transform (Coords _lng _lat) = Coords lng1 lat1
    where
      lng1  = (_lng / originShift) * 180
      lat1' = (_lat / originShift) * 180
      lat1  = 180 / pi * (2 * atan ( exp ( lat1' * pi / 180) ) - pi / 2)

originShift :: Double
originShift = 2 * pi * 6378137 / 2

bboxSphericalMercator :: BoundingBox SphericalMercator
bboxSphericalMercator = BoundingBox
  { boundingBoxWestNorth = fromTileIndex (TileIndex 0 0 0)
  , boundingBoxEastSouth = fromTileIndex (TileIndex 0 1 1)
  }

tileMatrixSphericalMercator :: TileMatrix SphericalMercator
tileMatrixSphericalMercator = TileMatrix
  { tileMatrixTileWidth = 256
  , tileMatrixTileHeight = 256
  , tileMatrixExtent = bboxSphericalMercator
  }

instance HasTileIndex WGS84 where
  fromTileIndex TileIndex{z,x,y} = Coords _lng _lat
    where
      _lng  = fromIntegral x / pow2 z * 360 - 180
      _lat  = 180 / pi * atan(0.5 * (exp n - exp (-n)))
      n    = pi - 2 * pi * fromIntegral y / pow2 z

  toTileIndex z (Coords _lng _lat) = TileIndex{z,x,y}
    where
      x = truncate x'
      y = truncate y'
      x' = (_lng + 180) / 360 * pow2 z
      y' = (1 - log (tan (_lat * pi/180) + 1 / cos(_lat * pi/180)) / pi)
         / 2 * pow2 z


maxResolution :: TileMatrix crs -> Double
maxResolution
  TileMatrix { tileMatrixTileWidth  = fromIntegral -> tw
             , tileMatrixTileHeight = fromIntegral -> th
             , tileMatrixExtent = ext
             } = max (bboxWidth ext / tw) (bboxHeight ext / th)

resolutionForZoom :: TileMatrix crs -> Zoom -> Double
resolutionForZoom  tm z = maxResolution tm / pow2 z

resolutions :: TileMatrix crs -> [Double]
resolutions tm = map (resolutionForZoom tm) [0..]

fromTileIndexWith :: TileMatrix crs -> TileIndex -> Coords crs
fromTileIndexWith tm TileIndex{z,x,y} = Coords col row
  where
    col = tm^.extent.west + (fromIntegral (x * tm^.tileWidth) * res)
    row = tm^.extent.north + (fromIntegral (y * tm^.tileHeight) * (-res))
    res = resolutionForZoom tm z

data ReverseIntersectionPolicy = LowerTile | HigherTile

toTileIndexWithPolicy :: ReverseIntersectionPolicy -> TileMatrix crs -> Zoom -> Coords crs -> TileIndex
toTileIndexWithPolicy pol tm z (Coords _lng _lat) = TileIndex {z,x,y}
  where
    x = case pol of
      LowerTile -> ceiling x' - 1
      HigherTile -> floor x'
    y = case pol of
      LowerTile -> ceiling y' - 1
      HigherTile -> floor y'

    x' = fromIntegral x0' / fromIntegral (tm^.tileWidth) :: Double
    y' = fromIntegral y0' / fromIntegral (tm^.tileHeight) :: Double

    x0' = floor ( (_lng - x0) / resolution + adjust ) :: Int
    y0' = floor ( (y0   - _lat) / resolution - adjust ) :: Int

    resolution = resolutionForZoom tm z

    y0 = tm^.extent.north
    x0 = tm^.extent.west

    adjust = case pol of
      LowerTile -> 0.5
      HigherTile -> 0


toTileIndexWith :: TileMatrix crs -> Zoom -> Coords crs -> TileIndex
toTileIndexWith = toTileIndexWithPolicy HigherTile

instance HasTileIndex SphericalMercator where
  fromTileIndex = transform  . (fromTileIndex :: TileIndex -> Coords WGS84)
  toTileIndex z = toTileIndex z   . (transform :: Coords SphericalMercator -> Coords WGS84)

pow2 :: Int -> Double
pow2 = fromIntegral . (bit :: Int -> Int)



tileBoundingBox :: HasTileIndex crs => TileIndex -> BoundingBox crs
tileBoundingBox t@(TileIndex z x y) =
  BoundingBox {
    boundingBoxWestNorth = fromTileIndex t
  , boundingBoxEastSouth = fromTileIndex (TileIndex z (x+1) (y+1))
  }

tileBoundingBoxWith :: TileMatrix crs -> TileIndex -> BoundingBox crs
tileBoundingBoxWith tm t@(TileIndex z x y) =
  BoundingBox {
    boundingBoxWestNorth = fromTileIndexWith tm t
  , boundingBoxEastSouth = fromTileIndexWith tm (TileIndex z (x+1) (y+1))
  }

bboxWidth :: (Num a, HasEast o a, HasWest o a) => o -> a
bboxWidth bb = bb^.east - bb^.west
{-# INLINE bboxWidth #-}

bboxHeight :: (Num a, HasNorth o a, HasSouth o a) => o -> a
bboxHeight bb = bb^.north - bb^.south
{-# INLINE bboxHeight #-}
