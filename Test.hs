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

p:: NDims s => Vec (ND s) Double -> Point s ()
p v = Point v ()

postcard :: Polygon LocustView Colour
postcard = translatePoly (fmap ((-10)*) uvz) $  Poly [p orig, p uvx, p (uvx+uvy), p uvy] (1, 0, 0)

main = do
   initGlScreen
   print postcard
   displayPolygons [postcard]
   waitSecs 1

