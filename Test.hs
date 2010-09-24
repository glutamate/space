{-# OPTIONS_GHC -fglasgow-exts #-}
module Main where

import Space
import OpenGl
import Dims
import TNUtils

data LocustView

instance GLSpace LocustView where
     glsFrustrum _ =  (-0.2, 0.2, -0.15, 0.15, 0.163, 100.0)

type instance NDims LocustView = Three
 


box :: [Polygon LocustView Colour]
box = translates (scalarMul (-10.0) uvz) $ tags (1, 0, 0) $ unitBox  

main = do
   initGlScreen
   print box
   render box
   waitSecs 1

