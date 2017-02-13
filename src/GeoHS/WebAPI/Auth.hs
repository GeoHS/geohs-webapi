{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE NamedFieldPuns        #-}

module GeoHS.WebAPI.Auth where

import           GeoHS.WebAPI.Profile (UserName)

import           Data.Aeson     (FromJSON, ToJSON)
import           Data.Swagger   (ToSchema, ToParamSchema)
import           Data.Text      (Text)
import           Data.Proxy     (Proxy(..))
import           GHC.Generics   (Generic)
import           Servant.API
import           Servant.Auth.Server
import           Servant.PureScript (jsonParseUrlPiece, jsonParseHeader)

type Login = Text


data Credentials = Credentials
  { login    :: !Login
  , password :: !Text
  } deriving (Generic, Eq, FromJSON, ToSchema)

data Token = Token
  { tokenOwner  :: !UserName
  } deriving (Show, Generic, Eq, FromJSON, ToJSON, ToSchema)
instance ToJWT Token
instance FromJWT Token



data TokenRequest = TokenRequest
  { tokenLifetimeSeconds :: Int
  } deriving (Show, Generic, Eq, FromJSON, ToJSON, ToSchema)

data JWToken = JWToken Text
  deriving (Generic, Show, Eq, Ord, FromJSON, ToJSON, ToSchema, ToParamSchema)

unJWToken :: JWToken -> Text
unJWToken (JWToken t) = t
{-# INLINE unJWToken #-}

instance FromHttpApiData JWToken where
  parseUrlPiece x = either (const (jsonParseUrlPiece x)) Right (JWToken <$> parseUrlPiece x)
  parseHeader x = either (const (jsonParseHeader x)) Right (JWToken <$> parseHeader x)

instance MimeRender PlainText JWToken where
  mimeRender _ = mimeRender (Proxy :: Proxy PlainText) . unJWToken


data ResourcePermission ob
  = Get ob
  | Put ob
  | Delete ob
  | List   ob
  deriving (Generic, Read, Show, Eq, Ord, FromJSON, ToJSON, ToSchema)
instance (ToJSON ob, ToJWT ob) => ToJWT (ResourcePermission ob)
instance (FromJSON ob, FromJWT ob) => FromJWT (ResourcePermission ob)

data Resource
  = LayerR
  | UserR
  | ProfileR
  | AclR
  deriving (Generic, Read, Show, Eq, Ord, FromJSON, ToJSON, ToSchema)
instance ToJWT Resource
instance FromJWT Resource

type Permission = ResourcePermission Resource
