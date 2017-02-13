{-# LANGUAGE AutoDeriveTypeable    #-}
{-# LANGUAGE OverloadedLists       #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE NamedFieldPuns        #-}

module GeoHS.WebAPI.GeneratePS (generatePS) where

import           GeoHS.WebAPI

import           Control.Applicative
import           Control.Lens hiding (Context)
import           Data.Proxy
import           Language.PureScript.Bridge
import           Servant.PureScript
import           SpatialReference


--
-- Purescript bridge
--

generatePS :: FilePath -> IO ()
generatePS frontEndRoot = do
  let api     = Proxy :: Proxy API
  writePSTypes frontEndRoot (buildBridge myBridge) myTypes
  writeAPIModuleWithSettings psSettings frontEndRoot myBridgeProxy api


psSettings :: Servant.PureScript.Settings
psSettings = addReaderParam "Authorization" Servant.PureScript.defaultSettings
  & apiModuleName .~ "GeoHS.WebAPI"


bsBridge :: BridgePart
bsBridge = do
  typeName ^== "ByteString"
  pure psByteString

psByteString :: PSType
psByteString = TypeInfo {
    _typePackage = "purescript-base64"
  , _typeModule = "Text.Base64"
  , _typeName = "Base64"
  , _typeParameters = []
  }

myBridge :: BridgePart
myBridge = defaultBridge <|> bsBridge

data MyBridge

myBridgeProxy :: Proxy MyBridge
myBridgeProxy = Proxy

instance HasBridge MyBridge where
  languageBridge _ = buildBridge myBridge

myTypes :: [SumType 'Haskell]
myTypes = [ mkSumType (Proxy :: Proxy JWToken)
          , mkSumType (Proxy :: Proxy TokenRequest)
          , mkSumType (Proxy :: Proxy Credentials)
          , mkSumType (Proxy :: Proxy Token)
          , mkSumType (Proxy :: Proxy Profile)
          , mkSumType (Proxy :: Proxy ProfilePublicData)
          , mkSumType (Proxy :: Proxy Email)
          , mkSumType (Proxy :: Proxy Url)
          , mkSumType (Proxy :: Proxy Attribution)
          , mkSumType (Proxy :: Proxy WmsLayer)
          , mkSumType (Proxy :: Proxy Source)
          , mkSumType (Proxy :: Proxy MapnikStyle)
          , mkSumType (Proxy :: Proxy EPSG)
          , mkSumType (Proxy :: Proxy Layer)
          , mkSumType (Proxy :: Proxy LayerName)
          , mkSumType (Proxy :: Proxy LatLng)
          , mkSumType (Proxy :: Proxy Bounds)
          , mkSumType (Proxy :: Proxy WmtsRequest)
          , mkSumType (Proxy :: Proxy TileIndex)
          , mkSumType (Proxy :: Proxy Tile)
          , mkSumType (Proxy :: Proxy RGBA8)
          ]
