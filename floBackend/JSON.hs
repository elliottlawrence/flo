{-# LANGUAGE OverloadedStrings, TypeSynonymInstances, FlexibleInstances #-}
module JSON where

import FloGraph

import Data.Aeson
import Data.Aeson.Types
import qualified Data.IntMap as IntMap
import qualified Data.Vector as V
import Control.Monad

instance FromJSON FloGraph where
  parseJSON (Object v) = FloGraph <$> v .: "modules"
  parseJSON _ = mzero

instance FromJSON Module where
  parseJSON (Object v) = Module <$> v .: "name" <*> v .: "boxDefinitions"
  parseJSON _ = mzero

instance FromJSON BoxDef where
  parseJSON (Object v) = BoxDef <$> v .: "boxInterface" <*> v .: "boxes"
                         <*> v .: "cables" <*> v .: "boxDefinitions"
  parseJSON _ = mzero

instance FromJSON BoxInterface where
  parseJSON (Object v) = BoxInterface <$> v .: "name" <*> v .: "boxFlavor"
                         <*> v .: "inputs"
  parseJSON _ = mzero

instance FromJSON BoxInterfaceMap where
  parseJSON (Array a) = IntMap.fromList <$> mapM parseBox (V.toList a)
    where parseBox (Object v) = do key <- v .: "ID"
                                   val <- v .: "boxInterface"
                                   return (key, val)
          parseBox _ = mzero
  parseJSON _ = mzero

instance FromJSON BoxFlavor where
  parseJSON (String "Function") = pure Function
  parseJSON (String "Constructor") = pure Constructor
  parseJSON (String "Literal") = pure Literal
  parseJSON _ = mzero

instance FromJSON Cable where
  parseJSON (Object v) = (:-:) <$> v .: "output" <*> v .: "input"
  parseJSON _ = mzero

instance FromJSON Input where
  parseJSON (Object v) = Input <$> v .: "name" <*> v .: "parentID"
  parseJSON _ = mzero

instance FromJSON Output where
  parseJSON (Object v) = Output <$> v .: "parentID" <*> v .: "endInputName"
  parseJSON _ = mzero
