{-# LANGUAGE ScopedTypeVariables, UndecidableInstances #-} 
{-# OPTIONS_GHC -fglasgow-exts #-}
module OpenGl where

import Dims
import Space

import Graphics.Rendering.OpenGL hiding (Sink, get, Point)
import Graphics.UI.GLFW -- hiding (Sink, get)
import Control.Monad

class GLSpace s where 
     glsFrustrum :: s -> (Double,Double,Double,Double,Double, Double)

type Colour = (Double,Double,Double)

toGLColour :: Colour -> Color3 GLfloat
toGLColour (r,g,b) = Color3 (realToFrac r) (realToFrac g) (realToFrac b)
 
vertex3d :: (NDims s ~ Three) => Point s a -> Vertex3 GLfloat
vertex3d (Point (VCons x (VCons y (VCons z VNil))) _) = Vertex3 (realToFrac x) (realToFrac y) (realToFrac z)

class HasSpace r where
    type TheSpace r :: *
    toSpace :: TheSpace r ~ s => r -> s 
    toSpace = undefined 

instance HasSpace (Polygon s a) where
    type TheSpace (Polygon s a) = s

instance HasSpace t => HasSpace [t] where
    type TheSpace [t] = TheSpace t

class Renderable r where
    renderIt :: r -> IO ()

instance (GLSpace s, NDims s ~ Three) => Renderable (Polygon s Colour) where
    renderIt (Poly pts col) = do
         color $ toGLColour col
         renderPrimitive Polygon $ forM_ pts $ vertex . vertex3d 

instance Renderable r => Renderable [r] where
    renderIt = mapM_ renderIt


render :: (GLSpace s, NDims s ~ Three, HasSpace r, TheSpace r ~ s, Renderable r) => r -> IO ()
render x = do
 clear [ColorBuffer]
 matrixMode $= Projection
 loadIdentity
 let (f0,f1,f2,f3,f4, f5) = glsFrustrum $ toSpace x
 frustum f0 f1 f2 f3 f4 f5
 matrixMode $= Modelview 0
 loadIdentity
 renderIt x
 --threadDelay $ 300*1000
 swapBuffers 
         
initGlScreen = do
  initialize
  --when ("-aa" `elem` args) $ do
  openWindowHint $= (FSAASamples, 1)
  openWindow (Size 640 480) [
                  DisplayRGBBits 8 8 8,
                  DisplayAlphaBits 8,
                  DisplayDepthBits 24,
                  DisplayStencilBits 0
                 ] Window
  windowTitle $= "GLSpace display"
  swapInterval $= 1
  clearColor $= Color4 0 0 0 0
  clear [ColorBuffer]
  lineSmooth $= Enabled
  polygonSmooth $= Enabled
  hint PolygonSmooth $= Nicest
  hint LineSmooth $= Nicest
  hint PointSmooth $= Nicest
  --blend $= Enabled 
    --cullFace $= Just Back
  blendFunc $= (SrcAlphaSaturate, One)
  multisample $= Enabled 
  swapBuffers

