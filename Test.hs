{-# OPTIONS_GHC -fglasgow-exts #-}
module Main where

import Space
import OpenGl
import Dims
import TNUtils

data LocustView

instance GLSpace LocustView where
     glsFrustrum _ =  (-0.2, 0.2, -0.15, 0.15, 0.163, 100.0)

instance NDims LocustView where
   type ND LocustView = Three
 
unitBox :: (NDims s, ND s ~ Three) => [Polygon s ()]
unitBox = uB where
  fz = Poly [pnt orig, pnt uvx, pnt (uvx+uvy), pnt uvy] ()
  fx = Poly [pnt orig, pnt uvy, pnt (uvz+uvy), pnt uvz] ()
  fy = Poly [pnt orig, pnt uvz, pnt (uvz+uvx), pnt uvx] ()
  uB = [fz, fx, fy, translate uvz fz, translate uvx fx, translate uvy fy] 

box :: [Polygon LocustView Colour]
box = translates (scalarMul (-10.0) uvz) $ tags (1, 0, 0) $ unitBox  

main = do
   initGlScreen
   print box
   displayPolygons box
   waitSecs 1

