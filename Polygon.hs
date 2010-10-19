{-# OPTIONS_GHC -fglasgow-exts #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Polygon where

import Nats
import VectorsL
import Space

data Polygon n a where 
     Poly :: [Vec n Double] -> a -> Polygon n a 

instance Functor (Polygon n) where 
    fmap f (Poly pts x) = Poly pts $ f x
 
  
unitBox3 :: [Polygon Three ()]
unitBox3 = uB where
  fz = Poly [orig, uvx, (uvx+uvy), uvy] ()
  fx = Poly [orig, uvy, (uvz+uvy), uvz] ()
  fy = Poly [orig, uvz, (uvz+uvx), uvx] ()
  uB = [fz, fx, fy, translate uvz fz, translate uvx fx, translate uvy fy] 

polyTag :: a -> Polygon n () -> Polygon n a 
polyTag x (Poly p _) =  Poly p x

stretchPoly :: Nat n => Vec n Double -> Polygon n a -> Polygon n a
stretchPoly v (Poly pts a) = Poly (map g pts) a
   where g (vp) = vp*v

data Shape n a where
    Polys :: [Polygon n a] -> Shape n a
    Translate :: Vec n Double -> Shape n a -> Shape n a

instance Functor (Shape s) where 
    fmap f (Polys polys) = Polys $ map (fmap f) polys
    fmap f (Translate v shp) = Translate v $ fmap f shp

 
instance Transform Polygon where
    translate vec (Poly pts x) = Poly (map (+vec) pts) x

instance Transform Shape where
    translate vec shp = Translate vec shp 

instance Show a => Show (Polygon n a) where
    show (Poly pts x) = "Poly "++show pts ++ " "++show x



