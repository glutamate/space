{-# LANGUAGE ScopedTypeVariables, UndecidableInstances #-} 
{-# OPTIONS_GHC -fglasgow-exts #-}
module GLToImage where

import Nats
import VectorsL 
--import Volume
--import Polygon
import OpenGl
--import Image 
import GeneralizedSignals

import Foreign (allocaBytes, Ptr )
import Foreign.Storable
import Data.Word

 
import Graphics.Rendering.OpenGL 
import Graphics.UI.GLFW -- hiding (Sink, get)
import Control.Monad

foo :: GLsizei -> Int
foo x = fromInteger $ toInteger x

renderToImage :: (Renderable r) 
                 => GLScene -> r -> IO (Signal (Vec Two Int) Colour)
renderToImage gls r = do
  Size w h <- get windowSize
 
  render gls r
  allocaBytes (foo $ 4*w*h) $ \colData -> do
     readPixels (Position 0 0) (Size w h) (PixelData RGBA UnsignedByte colData)
     let mf v = do
         let pixn = 4*((v!0)*(foo w)+(v!1)) 
         r::Word8 <- peekElemOff colData pixn
         g <- peekElemOff colData $ pixn+1
         b <- peekElemOff colData $ pixn+2     
         return (realToFrac r, realToFrac g, realToFrac b)
     let delta = 1                            
         lims  = (0, (foo w) `vcons` (foo h) `vcons` vnil)
     fillIO delta lims lims mf  
   

 
{-      let pixelData fmt = PixelData fmt Float depthImage :: PixelData GLfloat
      readPixels (Position 0 0) shadowMapSize' (pixelData DepthComponent)
      (_, Size viewPortWidth _) <- get viewport
      windowPos (Vertex2 (fromIntegral viewPortWidth / 2 :: GLfloat) 0)
      drawPixels shadowMapSize' (pixelData Luminance)
      swapBuffers -}
