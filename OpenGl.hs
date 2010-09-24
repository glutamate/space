{-# LANGUAGE ScopedTypeVariables, UndecidableInstances #-} 
{-# OPTIONS_GHC -fglasgow-exts #-}
module OpenGl where

import Dims
import Space
import Volume
import Polygon
 
import qualified Graphics.Rendering.OpenGL as GL 
import Graphics.Rendering.OpenGL (($=))
import Graphics.UI.GLFW -- hiding (Sink, get)
import Control.Monad

--it is not a good idea to give scene info statically.
class GLSpace s where 
     glsFrustrum :: s -> (Double,Double,Double,Double,Double, Double)

data GLScene = GLScene {
     glFrustrum :: (Double,Double,Double,Double,Double, Double)
}

type Colour = (Double,Double,Double)

toGLColour :: Colour -> GL.Color3 GL.GLfloat
toGLColour (r,g,b) = GL.Color3 (realToFrac r) (realToFrac g) (realToFrac b)
 
vertex3d :: (NDims s ~ Three) => Point s a -> GL.Vertex3 GL.GLfloat
vertex3d (Point (x ::: y ::: z :::VNil) _) = GL.Vertex3 (realToFrac x) (realToFrac y) (realToFrac z)

class Renderable r where
    renderIt :: r -> IO ()

class Surface a where
      withSurface :: a -> IO () -> IO ()
instance Surface Colour where
      withSurface col ma = GL.color (toGLColour col) >> ma

instance (GLSpace s, NDims s ~ Three, Surface a) => Renderable (Polygon s a) where
    renderIt (Poly pts col) = withSurface col $ do
         GL.renderPrimitive GL.Polygon $ forM_ pts $ GL.vertex . vertex3d 

instance Renderable r => Renderable [r] where
    renderIt = mapM_ renderIt


instance (GLSpace s, NDims s ~ Three, Surface a) => Renderable (Cuboid s a) where
    renderIt (Cuboid v col) = withSurface col $ renderIt box
       where box :: [Polygon s a]
             box = map (polyTag col) $ map (stretchPoly v) $ unitBox3

instance (GLSpace s, NDims s ~ Three, Surface a) => Renderable (Spheroid s a) where
    renderIt (Spheroid (x:::y:::z:::VNil) col) = GL.preservingMatrix $ withSurface col $ do
         GL.scale x y z
         GL.renderQuadric (GL.QuadricStyle Nothing 
                                           GL.NoTextureCoordinates 
                                           GL.Outside 
                                           GL.FillStyle) 
                       $ GL.Sphere 1 10 10

instance (GLSpace s, NDims s ~ Three, Surface a, Renderable (r s a)) 
            => Renderable (Translated r s a) where
    renderIt (Translated (x:::y:::z:::VNil) vol) = GL.preservingMatrix $ do
         GL.translate $ GL.Vector3 x y z --(realToFrac x) (realToFrac y) (realToFrac z)
         renderIt vol
       
 
render :: (GLSpace s, NDims s ~ Three, HasSpace r, TheSpace r ~ s, Renderable r) => r -> IO ()
render x = do
 GL.clear [GL.ColorBuffer]
 GL.matrixMode $= GL.Projection
 GL.loadIdentity
 let (f0,f1,f2,f3,f4, f5) = glsFrustrum $ toSpace x
 GL.frustum f0 f1 f2 f3 f4 f5
 GL.matrixMode $= GL.Modelview 0
 GL.loadIdentity
 renderIt x
 --threadDelay $ 300*1000
 swapBuffers 
         
initGlScreen = do
  initialize
  --when ("-aa" `elem` args) $ do
  --openWindowHint $= (FSAASamples, 1)
  openWindow (GL.Size 640 480) [
                  DisplayRGBBits 8 8 8,
                  DisplayAlphaBits 8,
                  DisplayDepthBits 24,
                  DisplayStencilBits 0
                 ] Window
  windowTitle $= "GLSpace display"
  swapInterval $= 1
  GL.clearColor $= GL.Color4 0 0 0 0
  GL.clear [GL.ColorBuffer]
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

