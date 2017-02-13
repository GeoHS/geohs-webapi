{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists       #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE QuasiQuotes           #-}


module GeoHS.WebAPI.Profile where

import           Control.Lens
import           Data.Aeson        (FromJSON, ToJSON, Value)
import           Data.Swagger
import           Data.Proxy        (Proxy(..))
import           Data.Text         (Text)
import           Data.String       (IsString)
import           GHC.Generics      (Generic)
import           NeatInterpolation (text)


type UserName = Text

newtype Email = Email Text
  deriving (Generic, Show, Eq, FromJSON, ToJSON)

emailRegExp :: Text
emailRegExp = [text|"^(([^<>()\[\]\\.,;:\s@"]+(\.[^<>()\[\]\\.,;:\s@"]+)*)|(".+"))@((\[[0-9]{1,3}\.[0-9]{1,3}\.[0-9]{1,3}\.[0-9]{1,3}])|(([a-zA-Z\-0-9]+\.)+[a-zA-Z]{2,}))$"|]

instance ToSchema Email where
  declareNamedSchema _ = do
    return $ NamedSchema (Just "Email") $ toSchema (Proxy :: Proxy Text)
      & pattern ?~ emailRegExp

data Profile
  = Profile
    { profileOwner      :: UserName
    , profileEmail      :: Maybe Email
    , profilePublicData :: ProfilePublicData
    }
  | PublicProfile
    { profileOwner      :: UserName
    , profilePublicData :: ProfilePublicData
    }
  deriving (Show, Generic, Eq, FromJSON, ToJSON)


tagSchema
  :: forall t. IsString t
  => [Value] -> (t, Referenced Schema)
tagSchema ts = ("tag", Inline (toSchema (Proxy :: Proxy Text) & enum_ ?~ ts))

instance ToSchema Profile where
  declareNamedSchema _ = do
    ownerSchema <- declareSchemaRef (Proxy :: Proxy UserName)
    emailSchema <- declareSchemaRef (Proxy :: Proxy (Maybe Email))
    publicSchema <- declareSchemaRef (Proxy :: Proxy ProfilePublicData)
    return $ NamedSchema (Just "Profile") $ mempty
      & type_ .~ SwaggerObject
      & properties .~
          [ tagSchema ["Profile", "PublicProfile"]
          , ("profileOwner", ownerSchema)
          , ("profilePublicData", publicSchema)
          , ("profileEmail", emailSchema)
          ]
      & required .~ [ "tag", "profileOwner", "profilePublicData" ]

toPublicProfile :: Profile -> Profile
toPublicProfile Profile{profileOwner,profilePublicData} =
  PublicProfile{profileOwner,profilePublicData}
toPublicProfile p = p

data ProfilePublicData = ProfilePublicData
  { profilePublicEmail :: Maybe Email
  } deriving (Show, Generic, Eq, FromJSON, ToJSON)

instance ToSchema ProfilePublicData where
  declareNamedSchema _ = do
    emailSchema <- declareSchemaRef (Proxy :: Proxy (Maybe Email))
    return $ NamedSchema (Just "ProfilePublicData") $ mempty
      & type_ .~ SwaggerObject
      & properties .~
          [ ("profilePublicEmail", emailSchema)
          ]

defaultPublicData :: ProfilePublicData
defaultPublicData = ProfilePublicData
  { profilePublicEmail = Nothing
  }
