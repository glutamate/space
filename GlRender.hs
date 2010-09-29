{-# LANGUAGE ScopedTypeVariables, UndecidableInstances #-} 
{-# OPTIONS_GHC -fglasgow-exts #-}
module GlRender where

import Dims
import Space
import Volume
import Polygon
 
import qualified Graphics.Rendering.OpenGL as GL 
import Graphics.Rendering.OpenGL (($=))
import Graphics.UI.GLFW -- hiding (Sink, get)
import Control.Monad


instance (GLSpace s, NDims s ~ Three, Surface a) => Renderable (Polygon s a) where
    renderIt (Poly pts col) = withSurface col $ do
         GL.renderPrimitive GL.Polygon $ forM_ pts $ GL.vertex . vertex3d 

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
                       $ GL.Sphere 1 50 50

instance (GLSpace s, NDims s ~ Three, Surface a, Renderable (r s a)) 
            => Renderable (Translated r s a) where
    renderIt (Translated (x:::y:::z:::VNil) vol) = GL.preservingMatrix $ do
         GL.translate $ GL.Vector3 x y z --(realToFrac x) (realToFrac y) (realToFrac z)
         renderIt vol


instance (GLSpace s, NDims s ~ Three, Surface a, Renderable (r s a)) 
            => Renderable (Union r s a) where
    renderIt (Translated (x:::y:::z:::VNil) vol) = GL.preservingMatrix $ do
         GL.translate $ GL.Vector3 x y z --(realToFrac x) (realToFrac y) (realToFrac z)
         renderIt vol
       
