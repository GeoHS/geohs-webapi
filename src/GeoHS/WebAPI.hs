{-# OPTIONS_GHC -fno-warn-orphans  #-}
{-# LANGUAGE AutoDeriveTypeable    #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}

module GeoHS.WebAPI (
  module GeoHS.WebAPI
, module Export
) where

import           GeoHS.Geometry          as Export
import           GeoHS.WebAPI.Image      as Export
import           GeoHS.WebAPI.Maps.API   as Export (GetTile, WmtsApi, WmtsRequest(..))
import           GeoHS.WebAPI.Maps.Layer as Export
import           GeoHS.WebAPI.Maps.Tile  as Export
import           GeoHS.WebAPI.Auth       as Export
import           GeoHS.WebAPI.Profile    as Export

import           Control.Lens hiding ((.=))
import           Data.Attoparsec.ByteString (parseOnly)
import           Data.Swagger as Swagger hiding (Header)
import           Data.Monoid ((<>))
import           Data.Proxy
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Servant.API hiding (BasicAuth)
import           Servant.Swagger
import           Servant.Foreign hiding (BasicAuth)
import           Servant.Auth
import           SpatialReference
import           Network.Wai.Ogc.Common (crsParser, renderCrs)

type Authenticated a = Auth '[JWT, BasicAuth] a 


type API = "api" :> (
      AuthAPIV0
 :<|> Authenticated Token :> APIV0
 )

--
-- Version 0 (unstable)
--
--

type AuthAPIV0
    = "login" :> ReqBody '[JSON] Credentials
    :> Post '[JSON, PlainText] JWToken

type APIV0
    = "token" :> "new" :> ReqBody '[JSON] TokenRequest
    :> Post '[JSON, PlainText] JWToken
 :<|> "layers" :> LayerAPI
 :<|> Capture "owner" UserName :> "layers" :> LayerAPI
 :<|> ProfileApi

type ProfileApi = Capture "owner" UserName :> "profile" :> (
       Get '[JSON] Profile
  :<|> ReqBody '[JSON] Profile :> Put '[JSON] Profile
  )

type CrudApi idName id ob
    = Get '[JSON] [ob]
 :<|> ReqBody '[JSON] ob :> Post '[JSON] ob
 :<|> Capture idName id :> CrudObjectApi ob

type CrudObjectApi ob
    = Get '[JSON] ob
 :<|> ReqBody '[JSON] ob :> Put '[JSON] ob
 :<|> DeleteNoContent '[JSON] ()

type LayerAPI
    = CrudApi "name" LayerName Layer
 :<|> Capture "name" LayerName
      :> "tile"
      :> GetTile '[PNG8 'Nothing, WEBP 'Nothing, JSON] Tile
 :<|> Capture "name" LayerName
      :> "tms"
      :> Capture "srs" Crs
      :> GetTile '[PNG8 'Nothing, WEBP 'Nothing, JSON] Tile
 :<|> Capture "name" LayerName
      :> "wmts"
      :> Capture "srs" Crs
      :> WmtsApi '[PNG8 'Nothing, WEBP 'Nothing, JSON] Tile
 :<|> Capture "name" LayerName
      :> "wms"
      :> QueryParam "SRS" Crs
      :> QueryParam "BBOX" Bounds
      :> QueryParam "WIDTH" Int
      :> QueryParam "HEIGHT" Int
      :> Get [PNG8 'Nothing, WEBP 'Nothing, JSON] Tile



addParam :: Param -> Swagger -> Swagger
addParam param = allOperations.parameters %~ (Inline param :)

addDefaultResponse403 :: ParamName -> Swagger -> Swagger
addDefaultResponse403 pname = setResponseWith (\old _new -> alter403 old) 403 (return response403)
  where
    sname = markdownCode pname
    description403 = "Forbidden " <> sname
    alter403 = Swagger.description %~ (<> (" or " <> sname))
    response403 = mempty & Swagger.description .~ description403

addDefaultResponse401 :: ParamName -> Swagger -> Swagger
addDefaultResponse401 pname = setResponseWith (\old _new -> alter401 old) 401 (return response401)
  where
    sname = markdownCode pname
    description401 = "Unauthorized " <> sname
    alter401 = Swagger.description %~ (<> (" or " <> sname))
    response401 = mempty & Swagger.description .~ description401

-- | Format given text as inline code in Markdown.
markdownCode :: Text -> Text
markdownCode s = "`" <> s <> "`"


instance (HasForeignType lang ftype Text, HasForeign lang ftype api)
  => HasForeign lang ftype (Authenticated a :> api) where
  type Foreign ftype (Authenticated a :> api) = Foreign ftype api

  foreignFor lang Proxy Proxy req =
    foreignFor lang Proxy subP $ req & reqHeaders <>~ [HeaderArg arg]
    where
      hname = "Authorization"
      arg   = Arg
        { _argName = PathSegment hname
        , _argType  = typeFor lang (Proxy :: Proxy ftype) (Proxy :: Proxy Text) }
      subP  = Proxy :: Proxy api

instance HasSwagger sub => HasSwagger (Authenticated a :> sub) where
  toSwagger _ = toSwagger (Proxy :: Proxy sub)
    & addParam paramAuthHeader
    & addDefaultResponse401 "Authorization"
    & addDefaultResponse403 "Authorization"
    where
      paramAuthHeader :: Param
      paramAuthHeader = mempty
        & Swagger.name .~ "Authorization"
        & schema .~ ParamOther (mempty
            & in_ .~ ParamHeader
            & paramSchema .~ authHeaderSchema
            )

      authHeaderSchema = toParamSchema (Proxy :: Proxy Text)
        & pattern ?~ "(Bearer (.+)|.+)"


instance ToParamSchema Crs where
  toParamSchema _ = toParamSchema (Proxy :: Proxy Text)
    & pattern ?~ "(EPSG:\\d+)"

instance FromHttpApiData Crs where
  parseUrlPiece = either (Left . T.pack) Right . parseOnly crsParser . T.encodeUtf8
  parseHeader = either (Left . T.pack) Right . parseOnly crsParser

instance ToHttpApiData Crs where
  toUrlPiece = T.decodeUtf8 . renderCrs
  toHeader = renderCrs
