{-# OPTIONS_GHC -fglasgow-exts #-}
module Main where

import Space
import OpenGl
import Nats
import TNUtils
import Polygon
import Volume 
import VectorsL
import GlRender
import GLToImage
import Image

locustScene = GLScene (-0.2, 0.2, -0.15, 0.15, 0.163, 100.0) (0,0,1)

 
red = (255,0,0)
blue = (0,0,255)

away = scalarMul (-10.0) uvz
up = scalarMul (5.0) uvy
left= scalarMul (5.0) uvx



box :: [Polygon Three Colour]
box = translates away $ tags red $ unitBox3

box1 :: Cuboid Three Colour
box1 = Cuboid 1 red

sphere :: Spheroid Three Colour
sphere = Spheroid 1 red


main = do
   initGlScreen
--   render locustScene box
--   waitSecs 0.1 
--   render locustScene $ Translated away box1
--   waitSecs 0.1
--   render locustScene $ Translated away sphere
--   waitSecs 0.1
   im<- renderToImage locustScene $ Translated (away+up+left) sphere
   savePNG "test4.png" $  im
 
