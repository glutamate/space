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

szToInt :: GLsizei -> Int
szToInt x = fromInteger $ toInteger x

renderToImage :: (Renderable r) 
                 => GLScene -> r -> IO (Signal (Vec Two Int) Colour)
renderToImage gls r = do
  Size w h <- get windowSize
  print (w,h)
  render gls r
  allocaBytes (szToInt $ 3*w*h) $ \colData -> do
     readPixels (Position 0 0) (Size w h) (PixelData RGB UnsignedByte colData)
     let c b = realToFrac b/255
     let mf v = do
         let pixn = 3*((v!0)+(v!1)*(szToInt w)) 
--         print (v,pixn)
         r::Word8 <- peekElemOff colData pixn
         g <- peekElemOff colData $ pixn+1
         b <- peekElemOff colData $ pixn+2     
         return (c r, c g, c b)
     let delta = 1
         lims  = (0, szToInt (w-1) `vcons` szToInt (h-1) `vcons` vnil)
     fillIO delta lims lims mf  

