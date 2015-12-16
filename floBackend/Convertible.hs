{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
module Convertible where

class Convertible a b where
  convert :: a -> b

instance (Convertible a b, Functor f) => Convertible (f a) (f b) where
  convert = fmap convert
