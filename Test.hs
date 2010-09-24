{-# OPTIONS_GHC -fglasgow-exts #-}
module Main where

import Space
import OpenGl
import Dims
import TNUtils
import Polygon
import Volume 

data LocustView

instance GLSpace LocustView where
     glsFrustrum _ =  (-0.2, 0.2, -0.15, 0.15, 0.163, 100.0)

type instance NDims LocustView = Three
 
red = (1,0,0)

away = scalarMul (-10.0) uvz

box :: [Polygon LocustView Colour]
box = translates away $ tags red $ unitBox3

box1 :: Cuboid LocustView Colour
box1 = Cuboid (1:::1.::1) red

sphere :: Spheroid LocustView Colour
sphere = Spheroid (1:::1.::1) red


main = do
   initGlScreen
   print box
   render box
   waitSecs 1 
   render $ Translated away box1
   waitSecs 1
   render $ Translated away sphere
   waitSecs 1
