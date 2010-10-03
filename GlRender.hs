{-# LANGUAGE ScopedTypeVariables, UndecidableInstances #-} 
{-# OPTIONS_GHC -fglasgow-exts #-}
module GlRender where

import Nats
import Space
import Volume
import Polygon
import VectorsL
 
import OpenGl
import qualified Graphics.Rendering.OpenGL as GL 
import Graphics.Rendering.OpenGL (($=))
import Graphics.UI.GLFW -- hiding (Sink, get)
import Control.Monad
 
v ! i = vecIx i v

instance Surface a => Renderable (Polygon Three a) where
    renderIt (Poly pts col) = withSurface col $ do
         GL.renderPrimitive GL.Polygon $ forM_ pts $ GL.vertex . vertex3d 

instance Surface a => Renderable (Cuboid Three a) where
    renderIt (Cuboid v col) = withSurface col $ renderIt box
       where --box :: [Polygon Three a]
             box = map (polyTag col) $ map (stretchPoly v) $ unitBox3

instance Surface a => Renderable (Spheroid Three a) where
    renderIt (Spheroid v col) = GL.preservingMatrix $ withSurface col $ do
         GL.scale (v!0) (v!1) (v!2)
         GL.renderQuadric (GL.QuadricStyle Nothing 
                                           GL.NoTextureCoordinates 
                                           GL.Outside 
                                           GL.FillStyle) 
                       $ GL.Sphere 1 50 50
                       

instance (Surface a, Renderable (r Three a)) 
            => Renderable (Translated r Three a) where
    renderIt (Translated v vol) = GL.preservingMatrix $ do
         GL.translate $ GL.Vector3 (v!0) (v!1) (v!2) 
         renderIt vol


instance (Surface a, Renderable (r1 Three a), Renderable (r2 Three a)) 
            => Renderable (Union r1 r2 Three a) where
    renderIt (Union v1 v2) = renderIt v1 >> renderIt v2
       
