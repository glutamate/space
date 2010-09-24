{-# LANGUAGE ScopedTypeVariables, UndecidableInstances #-} 
{-# OPTIONS_GHC -fglasgow-exts #-}
module OpenGl where

import Dims
import Space
import Volume

import Graphics.Rendering.OpenGL hiding (Sink, get, Point)
import Graphics.UI.GLFW -- hiding (Sink, get)
import Control.Monad

class GLSpace s where 
     glsFrustrum :: s -> (Double,Double,Double,Double,Double, Double)

type Colour = (Double,Double,Double)

toGLColour :: Colour -> Color3 GLfloat
toGLColour (r,g,b) = Color3 (realToFrac r) (realToFrac g) (realToFrac b)
 
vertex3d :: (NDims s ~ Three) => Point s a -> Vertex3 GLfloat
vertex3d (Point (x ::: y ::: z :::VNil) _) = Vertex3 (realToFrac x) (realToFrac y) (realToFrac z)

class Renderable r where
    renderIt :: r -> IO ()

class Surface a where
      withSurface :: a -> IO () -> IO ()
instance Surface Colour where
      withSurface col ma = color (toGLColour col) >> ma

instance (GLSpace s, NDims s ~ Three, Surface a) => Renderable (Polygon s a) where
    renderIt (Poly pts col) = withSurface col $ do
         renderPrimitive Polygon $ forM_ pts $ vertex . vertex3d 

instance Renderable r => Renderable [r] where
    renderIt = mapM_ renderIt


instance (GLSpace s, NDims s ~ Three, Surface a) => Renderable (Cuboid s a) where
    renderIt (Cuboid v col) = withSurface col $ renderIt box
       where box :: [Polygon s a]
             box = map (polyTag col) $ map (stretchPoly v) $ unitBox
        

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
  --openWindowHint $= (FSAASamples, 1)
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
  --lineSmooth $= Enabled
  --polygonSmooth $= Enabled
  --hint PolygonSmooth $= Nicest
  --hint LineSmooth $= Nicest
  --hint PointSmooth $= Nicest
  --blend $= Enabled 
    --cullFace $= Just Back
  --blendFunc $= (SrcAlphaSaturate, One)
  --multisample $= Enabled 
  swapBuffers

