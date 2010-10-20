{-# LANGUAGE ScopedTypeVariables, UndecidableInstances #-} 
{-# OPTIONS_GHC -fglasgow-exts #-}
module OpenGl where

import Nats
--import Space
import VectorsL

import qualified Graphics.Rendering.OpenGL as GL 
import Graphics.Rendering.OpenGL (($=))
import Graphics.UI.GLFW -- hiding (Sink, get)
import Control.Monad

data GLScene = GLScene {
     glFrustrum :: (Double,Double,Double,Double,Double, Double),
     glBgColour :: (Int,Int,Int) 
}

type Colour = (Double,Double,Double)

toGLColour :: Colour -> GL.Color3 GL.GLfloat
toGLColour (r,g,b) = GL.Color3 (realToFrac r) (realToFrac g) (realToFrac b)
 
vertex3d :: Vec Three Double -> GL.Vertex3 GL.GLfloat
vertex3d v = GL.Vertex3 (i 0) (i 1) (i 2)
         where i n = realToFrac $ vecIx n v

class Renderable r where
    renderIt :: r -> IO ()

class Surface a where
      withSurface :: a -> IO () -> IO ()
instance Surface Colour where
      withSurface col ma = GL.color (toGLColour col) >> ma

instance Renderable r => Renderable [r] where
    renderIt = mapM_ renderIt

render :: (Renderable r) => GLScene -> r -> IO ()
render (GLScene frust (r,g,b)) x = do
 GL.clearColor $= GL.Color4 (fromIntegral r) (fromIntegral g) (fromIntegral b) 0
 GL.clear [GL.ColorBuffer]
 GL.matrixMode $= GL.Projection
 GL.loadIdentity
 let (f0,f1,f2,f3,f4, f5) = frust
 GL.frustum (realToFrac f0) 
            (realToFrac f1) 
            (realToFrac f2) 
            (realToFrac f3) 
            (realToFrac f4)
            (realToFrac f5)
 GL.matrixMode $= GL.Modelview 0
 GL.loadIdentity
 renderIt x
 --threadDelay $ 300*1000
 swapBuffers 
         
initGlScreen = do
  initialize
  --when ("-aa" `elem` args) $ do
  --openWindowHint $= (FSAASamples, 1)
  openWindow (GL.Size 200 200) [
                  DisplayRGBBits 8 8 8,
                  DisplayAlphaBits 8,
                  DisplayDepthBits 24,
                  DisplayStencilBits 0
                 ] Window
  windowTitle $= "GLSpace display"
  swapInterval $= 1
  GL.clearColor $= GL.Color4 0 0 0 0
  GL.clear [GL.ColorBuffer]
  GL.lineSmooth $= GL.Enabled
  GL.polygonSmooth $= GL.Enabled
  GL.hint GL.PolygonSmooth $= GL.Nicest
  GL.hint GL.LineSmooth $= GL.Nicest
  GL.hint GL.PointSmooth $= GL.Nicest
  GL.blend $= GL.Enabled 
  GL.cullFace $= Just GL.Back
  GL.blendFunc $= (GL.SrcAlphaSaturate, GL.One)
  GL.multisample $= GL.Enabled 
  swapBuffers

