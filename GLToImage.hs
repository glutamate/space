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
import Foreign.ForeignPtr
import Data.Word

import qualified Data.StorableVector as SV
import qualified Data.StorableVector.Base as SVB
import qualified Data.StorableVector.ST.Strict as SVST

 
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
  p <- mallocForeignPtrArray (fromIntegral $ w*h) 
  --allocaBytes (szToInt $ 3*w*h) $ \colData -> do
  withForeignPtr p $ \p'->
    readPixels (Position 0 0) (Size w h) (PixelData RGB UnsignedByte p')
{-     let mf v = do
         let pixn = 3*((v!0)+(v!1)*(szToInt w)) 
--         print (v,pixn)
         r <- peekElemOff colData pixn
         g <- peekElemOff colData $ pixn+1
         b <- peekElemOff colData $ pixn+2
         return ((r,g,b)::(Word8,Word8,Word8))-}
  let delta = 1
      lims  = (0, szToInt (w-1) `vcons` szToInt (h-1) `vcons` vnil)
  return $ Signal delta lims lims $ SVB.fromForeignPtr p (fromIntegral $ w*h)
     --fillIO delta lims lims mf  

