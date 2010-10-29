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
import System.Environment


ambient = (0.2,0.2,0.2)
diffuse = (0.8,0.8,0.8)
specular = (0.5,0.5,0.5)

lightLoc = scalarMul 30 (uvx+uvy)

locustScene = GLScene (-0.2, 0.2, -0.15, 0.15, 0.163, 100.0) 
               white ambient diffuse specular lightLoc

 
red = (255,0,0)
blue = (0,0,255)
green = (0,255,0)

white = (255, 255, 255)

away = scalarMul (-3.0) uvz
up = scalarMul (5.0) uvy
left= scalarMul (5.0) uvx


box :: Quads Three Colour
box =  tag red unitBox3

box1 :: Cuboid Three Colour
box1 = Cuboid 1 red

sphere :: Spheroid Three Colour
sphere = Spheroid 1 red

main = do 
     getArgs >>= dispatch

dispatch ["display"] = display
dispatch ["reduce"] = reduce 
dispatch _ = display

reduce = do
   img <- loadJPEG "redcube.jpg"
   savePNG "redcube.png" img 


display = do
   initGlScreen
--   render locustScene box
--   waitSecs 1
--   render locustScene $ Translated away box1
--   waitSecs 0.1
   render locustScene $ Translated away sphere
   waitSecs 0.1
   im<- renderToImage locustScene $ Translated away $ Rotated 45 left $  Rotated (45) up $ box1
   --waitSecs 10
   savePNG "test4.png" $  im
 
