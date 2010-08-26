module OpenGl where

import Dims
import Space

import Graphics.Rendering.OpenGL hiding (Sink, get)
import Graphics.UI.GLFW -- hiding (Sink, get)

class GLSpace a where 
     glsFrustrum :: a -> (Double,Double,Double,Double,Double)

data LocustView

instance GLSpace LocustView where
     glsFrustrum _ =  (-0.2, 0.2, -0.15, 0.15, 0.163, 100.0)

instance NDims GLSpace Three

type Colour = (Double,Double,Double)

--vertex3d :: NDims s Three => Point s a -> something
vertex3d (Vec (VCons x (VCons y (VCons z VNil)))) = vertex3 x y z

displayPolygons :: GLSpace s => [Polygon s Colour] -> IO ()
displayPolygons polys = do
 clear [ColorBuffer]
 matrixMode $= Projection
 loadIdentity
 let (f0,f1,f2,f3,f4) = glsFrustrum (undefined::s) 
 frustum f0 f1 f2 f3 f4
 matrixMode $= Modelview 0
 loadIdentity
 forM_ polys $ \Poly pts (r,g,b) -> do
        color $ Color3 (realToFrac r) (realToFrac g) (realToFrac b)
        renderPrimitive Polygon $ forM_ pts $ vertex . vertex3d 
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
--  swapInterval $= 1
  clearColor $= Color4 0 0.23 0 0
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

