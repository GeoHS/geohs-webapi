{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}

module GeoHS.WebAPI.Maps.Layer where

import GeoHS.Geometry

import Data.Aeson (FromJSON, ToJSON)
import Data.Proxy
import Data.Maybe (fromJust)
import Data.Text (Text)
import           Data.Swagger   (ToSchema(..))
import GHC.Generics (Generic)
import Control.Lens
import Control.Lens.TH (makeFields)
import SpatialReference
import Data.Swagger
import Web.HttpApiData (FromHttpApiData(..))

type Buffer = Int

newtype Url = Url Text
  deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON, ToSchema)

data Attribution = Attribution
  { attributionAuthor :: !Text
  , attributionUrl    :: !(Maybe Url)
  } deriving (Show, Eq, Generic, ToJSON, FromJSON, ToSchema)
makeFields ''Attribution

newtype LayerName = LayerName Text
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

instance FromHttpApiData LayerName where
  parseUrlPiece = fmap LayerName . parseUrlPiece
  parseQueryParam = fmap LayerName . parseQueryParam


instance ToSchema LayerName where
  declareNamedSchema _ = do
    let textSchema = toSchema (Proxy :: Proxy Text)
    return $ NamedSchema (Just "LayerName") $ textSchema
      & pattern ?~ "([ ]+)" --FIXME

instance ToParamSchema LayerName where
  toParamSchema _ = toParamSchema (Proxy :: Proxy Text)
    & pattern ?~ "([ ]+)"

data WmsLayer = WmsLayer
  { wmsLayerName  :: !LayerName
  , wmsLayerStyle :: !Text
  } deriving (Show, Eq, Generic, ToJSON, FromJSON, ToSchema)
makeFields ''WmsLayer

data MapnikStyle
  = MapnikStylePath   !FilePath
  | MapnikStyleString !Text
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

instance ToSchema MapnikStyle where
  declareNamedSchema _ = do
    let textSchema = toSchema (Proxy :: Proxy Text)
    return $ NamedSchema (Just "MapnikStyle") $ mempty
      & type_ .~ SwaggerObject
      & properties .~
          [ ("tag", Inline $ textSchema
              & enum_ ?~ ["MapnikStylePath", "MapnikStyleString"]
            )
          , ("contents", Inline textSchema)
          ]
      & required .~ [ "tag", "contents" ]

data Source
  = WmsSource
    { wmsSourceUrl       :: !Url
    , wmsSourceExtraUrls :: ![Url]
    , wmsSourceLayers    :: ![WmsLayer]
    }
  | TileSource
    { tileSourceUrl       :: !Url
    , tileSourceExtraUrls :: ![Url]
    }
  | MapnikSource
    { mapnikSourceStyle  :: !MapnikStyle
    } deriving (Show, Eq, Generic, ToJSON, FromJSON)
makeFields ''Source

instance ToSchema Source where
  declareNamedSchema _ = do
    urlSchema <- declareSchemaRef (Proxy :: Proxy Url)
    urlListSchema <- declareSchemaRef (Proxy :: Proxy [Url])
    wmsLayerListSchema <- declareSchemaRef (Proxy :: Proxy [WmsLayer])
    mapnikStyleSchema <- declareSchemaRef (Proxy :: Proxy MapnikStyle)
    let textSchema = toSchema (Proxy :: Proxy Text)
    return $ NamedSchema (Just "Source") $ mempty
      & type_ .~ SwaggerObject
      & properties .~
          [ ("tag", Inline $ textSchema
              & enum_ ?~ ["WmsSource", "TileSource","MapnikSource"]
            )
          , ("wmsSourceUrl", urlSchema)
          , ("wmsSourceExtraUrls", urlListSchema)
          , ("wmsSourceLayers", wmsLayerListSchema)
          , ("tileSourceUrl", urlSchema)
          , ("tileSourceExtraUrls", urlListSchema)
          , ("mapnikSourceStyle", mapnikStyleSchema)
          ]
      & required .~ [ "tag" ]


instance ToSchema EPSG where
  declareNamedSchema _ = do
    return $ NamedSchema (Just "EPSG") $
      toSchema (Proxy :: Proxy Int)
        & minimum_ .~ Just 0


data Layer = Layer
  { layerTitle        :: !Text
  , layerSource       :: !Source
  , layerDescription  :: !(Maybe Text)
  , layerAttribution  :: !Attribution
  , layerExtraAttributions  :: ![Attribution]
  , layerBufferSize         :: !(Maybe Buffer)
  , layerBounds       :: !Bounds
  , layerEpsg         :: !EPSG
  , layerQueryable    :: !Bool
  , layerOpacity      :: !Double
  , layerEnableRetina  :: !Bool
  , layerIsOverlay    :: !Bool
  } deriving (Show, Generic, ToJSON, FromJSON, ToSchema)
makeFields ''Layer

mapnikLayer :: Text -> MapnikStyle -> Attribution -> Layer
mapnikLayer title_ style_ attribution_ = Layer
  { layerTitle = title_
  , layerSource = MapnikSource style_
  , layerDescription = Nothing
  , layerAttribution = attribution_
  , layerExtraAttributions = []
  , layerBufferSize = Nothing
  , layerBounds = emptyBounds
     & southWest .~ latLng (-90) (-180)
     & northEast .~ latLng 90 180
  , layerEpsg = fromJust (mkEPSG 4326)
  , layerQueryable = False
  , layerOpacity   = 1
  , layerEnableRetina = False
  , layerIsOverlay = True
  }
