{-# LANGUAGE ScopedTypeVariables, UndecidableInstances #-} 
{-# OPTIONS_GHC -fglasgow-exts #-}
module GLToImage (renderToImage) where

import VectorsL 
import OpenGl
import GeneralizedSignals

import qualified Data.StorableVector as SV
import qualified Data.StorableVector.Base as SVB
 
import Graphics.Rendering.OpenGL 
import Graphics.UI.GLFW -- hiding (Sink, get)
import Image

szToInt :: GLsizei -> Int
szToInt = fromIntegral

renderToImage :: (Renderable r) 
                 => GLScene -> r -> IO (Image Colour)
renderToImage gls r = do
  Size w h <- get windowSize
  let delta = 1
      lims  = (0, szToInt (w-1) `vcons` szToInt (h-1) `vcons` vnil)
  render gls r
  fmap (Signal delta lims lims) $ 
       SVB.create (fromIntegral $ w*h) $ 
           readPixels (Position 0 0) (Size w h) .  PixelData RGB UnsignedByte


