{-# OPTIONS_GHC -fglasgow-exts #-}

module Space where

import Dims
import Data.Array.Storable
import Foreign.Storable


data Point s where
     Point :: NDims s n => Vec n Double -> Point s

data Image s a where
     ImArray :: (Storable a, NDims s n) => StorableArray (Int,Int) a -> Image s a
