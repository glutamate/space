{-# LANGUAGE EmptyDataDecls, UndecidableInstances, ScopedTypeVariables #-}
{-# OPTIONS_GHC -fglasgow-exts #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Image where

import qualified Data.StorableVector  as SV
import Foreign.Storable
import Dims 
import qualified Graphics.GD.ByteString as GD
import Control.Monad
import Data.Bits
import Foreign.Storable.Tuple

data Image s a where
     ImArray :: (Storable a) => Vec (NDims s) Int -> SV.Vector a -> Image s a
     ImFmap :: (a->b) -> Image s a -> Image s b
     ImF :: (Vec (NDims s) Int -> b) -> Image s b

at :: (NDims s ~ n) => Image s a -> Vec n Int -> a
at (ImFmap f im) v = f $ im `at` v
at (ImArray vsz arr) vix = arr `SV.index` vec2ImIx vix vsz -- (w*y+x)
at (ImF f) v = f v

-- this is probably wrong for 3D. Need to raise to power? better now?
vec2ImIx :: Vec n Int -> Vec n Int -> Int 
vec2ImIx VNil _ = 0
vec2ImIx (x:::vix) (w :::vsz) = x+w*vec2ImIx vix vsz 
vec2ImIx _ _ = error "vec2ImIx"

imageSize :: (NDims s ~ n) => Image s a -> Vec n Int
imageSize (ImFmap _ im) = imageSize im
imageSize (ImArray vsz _) = vsz

--unfoldrN :: Storable b => Int -> (a -> Maybe (b, a)) -> a -> (Vector b, Maybe a)	Source
fillImage :: (Storable a, NDims s ~ Two) => Int -> Int -> (Int -> Int -> a) -> Image s a
fillImage w h f = ImArray (w .:: h) $ fst $ SV.unfoldrN (w*h) unf (0,0) 
   where unf (x,y) | x < w = Just (f x y, (x+1, y))
                   | y < (h-1) = Just (f 0 (y+1), (1, y+1))
                   | otherwise = Nothing

fillImage':: forall b s n. (Storable b, NDims s ~ n) => Vec n Int -> (Vec n Int -> b) -> Image s b
fillImage' vsz f = ImArray vsz $ fst $ SV.unfoldrN (vec2ImIx vsz vsz) (unf vsz) (fmap (const 0) vsz)
   where unf :: Vec n Int -> Vec n Int -> Maybe (b, Vec n Int)
         --unf _ VNil = Nothing
         unf (w:::vsz) vix@(x:::vix') | x < w = Just (f vix, (x+1):::vix') {- | x < w = Just (f x y, (x+1, y))
                                      
                    | y < (h-1) = Just (f 0 (y+1), (1, y+1))
                   | otherwise = Nothing-}


fillImageLists :: (Storable a, NDims s ~ Two) => Int -> Int -> [[a]] -> Image s a
fillImageLists w h lsts = ImArray ( w .:: h ) $ SV.concat $ map SV.pack lsts 

loadPNG :: (NDims s~ Two) => FilePath -> IO (Image s (Int,Int,Int))
loadPNG fp = do
  gdim <- GD.loadPngFile fp
  (w,h) <- GD.imageSize gdim
  pts <- forM [0..w-1] $ \x-> forM [0..h-1] $ \y-> toColour `fmap` GD.getPixel (x,y) gdim
  return $ fillImageLists w h $ pts

savePNG :: (NDims s ~ Two) => FilePath -> Image s (Int,Int,Int) -> IO ()
savePNG fp img = do
  ((w ::: h::: VNil)::Vec Two Int) <- return $ imageSize img
  print (w,h)
  gdIm <- GD.newImage (w,h)
  forM_ [0..w-1] $ \x-> 
        forM_ [0..h-1] $ \y-> do
            let (r,g,b) = img `at` (x .:: y)            
            GD.setPixel (x,y) (GD.rgb r g b) gdIm 
  GD.savePngFile fp gdIm

ti = do in2d `fmap` loadPNG "test.png"
        return ()
tst = [x+y*10 | x<- [0..9], y <- [0..9]]

t2 = vec2ImIx (90.::99) (100.::100)

t1 = savePNG "test1.png" $ in2d $ fillImage 50 50 $ \x y -> c ((x+y) `mod` 3)
t3 = savePNG "test3.png" $ in2d $ fillImage 255 255 $ \x y -> (x, 0, y)

c 0 = (255,0,0)
c 1 = (0,255,0)
c 2 = (0,0,255)
c _ = (100,100,100)
data Flatland
type instance NDims Flatland = Two

type Id a = a -> a

in2d :: Id (Image Flatland s)
in2d = id

showBits :: Bits a => a -> String
showBits x = let nbs = bitSize x
                 s True = "1"
                 s False = "0"
             in concatMap (s . testBit x) [0..nbs]

red x =fromIntegral $ (x .&. (GD.rgb 255 0 0)) `shiftR` 16
green x =fromIntegral $  (x .&. (GD.rgb 0 255 0)) `shiftR` 8
blue x =fromIntegral $ (x .&. (GD.rgb 0 0 255))

toColour :: GD.Color -> (Int, Int, Int) 
toColour c = (red c, green c, blue c)