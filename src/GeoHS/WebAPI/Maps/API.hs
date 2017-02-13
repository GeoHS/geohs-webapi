{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE CPP           #-}
{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module GeoHS.WebAPI.Maps.API (
  GetTile
, WmtsApi
, WmtsRequest (..)
) where

import           Data.Aeson     (FromJSON, ToJSON)
import           Data.Swagger   (ToSchema, ToParamSchema)
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Servant.API
import           GHC.Generics   (Generic)

type GetTile formats output =
    Capture "z" Int
 :> Capture "x" Int
 :> Capture "y" Int
 :> Get formats output


type TileMatrixSet = Text

data WmtsRequest
  = GetTile
  deriving (Generic, Show, Eq, Ord, Enum, Bounded, FromJSON, ToJSON, ToSchema, ToParamSchema)

instance FromHttpApiData WmtsRequest where
  parseUrlPiece (T.toLower -> "gettile") = Right GetTile
  parseUrlPiece _                        = Left "Invalid WmtsRequest"
  parseHeader (T.toLower . T.decodeUtf8 -> "gettile") = Right GetTile
  parseHeader _                        = Left "Invalid WmtsRequest"


type WmtsApi formats output =
    QueryParam "REQUEST"       WmtsRequest
 :> QueryParam "TILEMATRIXSET" TileMatrixSet
 :> QueryParam "TILEMATRIX"    Int
 :> QueryParam "TILECOL"       Int
 :> QueryParam "TILEROW"       Int
 :> Get formats output
