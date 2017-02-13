{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module TestUtil where

import Data.Aeson
import Data.Swagger
import Data.Swagger.Schema.Validation (validateToJSON)
import Test.Hspec (Spec)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck
import Data.Typeable

validJSONProperty
  :: forall p a. (Show a, Typeable a, ToSchema a, Arbitrary a, ToJSON a)
  => p a -> Spec
validJSONProperty _ = prop ("validateToJSON (" ++ show (typeOf (undefined :: a)) ++ ")") $
  \(ob :: a) -> let cexample = show (validateToJSON ob, encode ob)
                in counterexample cexample $ null (validateToJSON ob)
