{-# LANGUAGE EmptyDataDecls, UndecidableInstances, ScopedTypeVariables #-}
{-# OPTIONS_GHC -fglasgow-exts #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Image where

import Foreign.Storable
import Nats 
import qualified Graphics.GD.ByteString as GD
import Control.Monad
import Data.Bits 
import Foreign.Storable.Tuple
import GeneralizedSignals
import VectorsL
import Data.Word
import Foreign


type Image n a = Signal (Vec n Int) a


loadPNG :: FilePath -> IO (Image Two (Word8,Word8,Word8))
loadPNG fp = do
  gdim <- GD.loadPngFile fp
  (w,h) <- GD.imageSize gdim
  let lims = (0, w `vcons` h `vcons` vnil)
  let mf v = toColour `fmap` GD.getPixel (v!0, v!1) gdim
  fillIO 1 lims lims mf

savePNG :: FilePath -> Image Two (Word8, Word8, Word8) -> IO ()
savePNG fp img = do
  let (_,v) = sigLims img
  let (w,h) = ((v!0)+1,(v!1)+1)
  gdIm <- GD.newImage (w,h)
  forM_ [0..w-1] $ \x-> 
        forM_ [0..h-1] $ \y-> do
            let (r,g,b) = img `at` (x `vcons` y`vcons`vnil)            
            GD.setPixel (x,y) (rgba (fromIntegral r) (fromIntegral g) (fromIntegral b) 0) gdIm 
  GD.savePngFile fp gdIm

ti = do loadPNG "test.png"
        return ()
tst = [x+y*10 | x<- [0..9], y <- [0..9]]

--t2 = vec2ImIx (90.::99) (100.::100)

t1 = savePNG "test1.png" $ 
     fill 1 (0,49) (0,49) $ \v -> c (((v!0)+(v!1)) `mod` 3)
t3 = savePNG "test3.png" $ 
     fill 1 (0,254) (0,254) $ \v -> (fromIntegral $ v!0, 0, fromIntegral $ v!1)

c 0 = (255,0,0)
c 1 = (0,255,0)
c 2 = (0,0,255)
c _ = (100,100,100)


tcol1 = GD.rgb 250 250 250
tcol2 = rgba 250 250 250 0


rgba :: Word8    -- ^ Red (0-255)
        -> Word8 -- ^ Green (0-255)
        -> Word8 -- ^ Blue (0-255)
        -> Word8 -- ^ Alpha (0-127), 0 is opaque, 127 is transparent
        -> GD.Color
rgba r g b a = 
    (fromIntegral a `shiftL` 24) .|.
    (fromIntegral r `shiftL` 16) .|.
    (fromIntegral g `shiftL` 8)  .|.
    fromIntegral b


showBits :: Bits a => a -> String
showBits x = let nbs = bitSize x
                 s True = "1"
                 s False = "0"
             in concatMap (s . testBit x) [0..nbs]

toColour :: GD.Color -> (Word8, Word8,Word8) 
toColour c = (red c, green c, blue c) where
  red x =fromIntegral $ (x .&. (GD.rgb 255 0 0)) `shiftR` 16
  green x =fromIntegral $  (x .&. (GD.rgb 0 255 0)) `shiftR` 8
  blue x =fromIntegral $ (x .&. (GD.rgb 0 0 255))

intColToGlCol :: (Double, Double, Double) ->
                 (Int, Int, Int) 
intColToGlCol (r, g, b) = (f r, f g, f b) where
   f int = round $  int * 255