{-# LANGUAGE ScopedTypeVariables, UndecidableInstances #-} 
{-# OPTIONS_GHC -fglasgow-exts #-}
module OpenGl where

import Nats
--import Space
import VectorsL
import Data.Word

import qualified Graphics.Rendering.OpenGL as GL 
import Graphics.Rendering.OpenGL (($=))
import Graphics.UI.GLFW -- hiding (Sink, get)
import Control.Monad

data GLScene = GLScene {
     glFrustrum :: (Double,Double,Double,Double,Double, Double),
     glBgColour :: Colour,
     ambientLight :: (Double,Double,Double),
     diffuseLight :: (Double,Double,Double),
     specularLight :: (Double,Double,Double),
     lightPos::  (Vec Three Double)

}

type Colour = (Word8,Word8,Word8)

toGLColour :: Colour -> GL.Color4 GL.GLubyte
toGLColour (r,g,b) = GL.Color4 (fromIntegral r) (fromIntegral g) (fromIntegral b) 0
 
toColF (r,g,b) = GL.Color4 (realToFrac r) (realToFrac g) (realToFrac b) 1

vertex3d :: Vec Three Double -> GL.Vertex3 GL.GLfloat
vertex3d v = GL.Vertex3 (i 0) (i 1) (i 2)
         where i n = realToFrac $ vecIx n v

vecToNormal :: Vec Three Double -> GL.Normal3 GL.GLfloat
vecToNormal v = GL.Normal3 (i 0) (i 1) (i 2)
         where i n = realToFrac $ vecIx n v


vertex4d :: Vec Three Double -> GL.Vertex4 GL.GLfloat
vertex4d v = GL.Vertex4 (i 0) (i 1) (i 2) 0
         where i n = realToFrac $ vecIx n v


class Renderable r where
    renderIt :: r -> IO ()

class Surface a where
      withSurface :: a -> IO () -> IO ()
instance Surface Colour where
      withSurface col ma = GL.color (toGLColour col) >> ma

instance Surface () where
      withSurface () ma = ma


instance Renderable r => Renderable [r] where
    renderIt = mapM_ renderIt

render :: (Renderable r) => GLScene -> r -> IO ()
render (GLScene frust (r,g,b) amb diff spec lightPosition) x = do
 GL.clearColor $= GL.Color4 (fromIntegral r) (fromIntegral g) (fromIntegral b) 255
 GL.clear [GL.ColorBuffer, GL.DepthBuffer]
 GL.matrixMode $= GL.Projection
 GL.loadIdentity
 GL.diffuse (GL.Light 0) $= toColF diff
 GL.ambient (GL.Light 0) $= toColF amb
 GL.specular (GL.Light 0) $= toColF spec
 GL.position  (GL.Light 0) $= vertex4d lightPosition
 GL.light (GL.Light 0) $= GL.Enabled

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
  openWindow (GL.Size 600 400) [
                  DisplayRGBBits 8 8 8,
                  DisplayAlphaBits 8,
                  DisplayDepthBits 24,
                  DisplayStencilBits 8
                 ] Window
  windowTitle $= "GLSpace display"
--  swapInterval $= 1
--  GL.clearColor $= GL.Color4 0 0 0 0
--  GL.clear [GL.ColorBuffer]
  GL.lineSmooth $= GL.Enabled
  GL.shadeModel $= GL.Smooth
  GL.clearDepth $= 1
  GL.depthFunc $= Just GL.Lequal -- http://twoguysarguing.wordpress.com/2010/02/20/opengl-and-haskell/
  GL.lighting $= GL.Enabled
  GL.colorMaterial $= Just (GL.Front, GL.Specular)
--  GL.polygonSmooth $= GL.Enabled
--  GL.hint GL.PolygonSmooth $= GL.Nicest
--  GL.hint GL.LineSmooth $= GL.Nicest
--  GL.hint GL.PerspectiveCorrection $= GL.Nicest
--  GL.blend $= GL.Enabled 
--  GL.cullFace $= Just GL.Back
  --GL.blendFunc $= (GL.SrcAlphaSaturate, GL.One)
--  GL.multisample $= GL.Enabled 
  swapBuffers

