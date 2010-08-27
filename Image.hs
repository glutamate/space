{-# OPTIONS_GHC -fglasgow-exts #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Image where

import qualified Data.StorableVector  as SV
import Foreign.Storable
import Dims 
import qualified Graphics.GD.ByteString as GD

data Image s a where
     ImArray :: (Storable a, NDims s) => Int -> Int -> SV.Vector a -> Image s a
     ImFmap :: (a->b) -> Image s a -> Image s b

at :: Image s a -> (Int,Int) -> a
at (ImFmap f im) xy = f $ im `at` xy
at (ImArray w h arr) (x,y) = arr `SV.index` (w*y+x)

--unfoldrN :: Storable b => Int -> (a -> Maybe (b, a)) -> a -> (Vector b, Maybe a)	Source
fillImage :: (Storable a, NDims s) => Int -> Int -> (Int -> Int -> a) -> Image s a
fillImage w h f = ImArray w h $ fst $ SV.unfoldrN (w*h) unf (0,0) 
   where unf (x,y) | x < w = Just (f x y, (x+1, y))
                   | y < (h-1) = Just (f 0 (y+1), (1, y+1))
                   | otherwise = Nothing

loadPNG :: (NDims s, ND s ~ Two) => FilePath -> IO (Image s (Int,Int,Int))
loadPNG fp = do
  undefined
