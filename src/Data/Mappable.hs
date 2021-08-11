{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_HADDOCK show-extensions #-}

module Data.Mappable
  ( Mappable(..)
  ) where

import Prelude hiding (map)

import qualified Data.Map as M
import qualified Data.Set as S

-- | Class Definition of Mappable
class Mappable f a b where
  map :: (a -> b) -> f a -> f b

-- | Mappable instance for Data.Set.Set
instance (Ord a, Ord b) => Mappable S.Set a b where
  map = S.map

-- | Mappable instance for Data.Map.Map
instance (Ord a, Ord b) => Mappable (M.Map k) a b where
  map = M.map
