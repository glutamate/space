{-# LANGUAGE ScopedTypeVariables, UndecidableInstances #-} 
{-# OPTIONS_GHC -fglasgow-exts #-}
module GLToImage where

import Dims
import Space
import Volume
import Polygon
import OpenGl
import Image 

import Foreign (allocaBytes, Ptr )
import Foreign.Storable

 
import Graphics.Rendering.OpenGL 
import Graphics.UI.GLFW -- hiding (Sink, get)
import Control.Monad

renderToImage :: (GLSpace s, NDims s ~ Three, NDims t ~ Two, HasSpace r, TheSpace r ~ s, Renderable r) 
                 => r -> IO (Image t Colour)
renderToImage x = do
  Size w h <- get windowSize

  allocaBytes (4*w*h) $ \colData -> do
     readPixels (Position 0 0) (Size w h) (PixelData RGBA UnsignedByte colData)
     r <- peekElemOff colData 0
     g <- peekElemOff colData 1
     b <- peekElemOff colData 2
     return (Color3 r g b)


{-      let pixelData fmt = PixelData fmt Float depthImage :: PixelData GLfloat
      readPixels (Position 0 0) shadowMapSize' (pixelData DepthComponent)
      (_, Size viewPortWidth _) <- get viewport
      windowPos (Vertex2 (fromIntegral viewPortWidth / 2 :: GLfloat) 0)
      drawPixels shadowMapSize' (pixelData Luminance)
      swapBuffers -}
