{-# OPTIONS_GHC -fglasgow-exts #-}

module Space where

import Dims
import Data.Array.Storable
import Foreign.Storable


data Point s a where
     Point :: NDims s n => Vec n Double -> a -> Point s a

data Image s a where
     ImArray :: (Storable a, NDims s n) => StorableArray (Int,Int) a -> Image s a
     ImFmap :: (a->b) -> Image s a -> Image s b

data Polygon s a where
     Poly :: [Point s ()] -> a -> Polygon s a



--data s :-> t where
--     Linear :: (NDims