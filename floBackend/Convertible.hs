{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
module Convertible where

import Control.Arrow

class Convertible a b where
  convert :: a -> b

instance (Convertible a b, Functor f) => Convertible (f a) (f b) where
  convert = fmap convert

instance (Convertible a b, Convertible c d) => Convertible (a,c) (b,d) where
  convert = convert *** convert
