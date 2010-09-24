{-# OPTIONS_GHC -fglasgow-exts #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Polygon where

import Dims 
import Space

data Polygon s a where 
     Poly :: [Point s ()] -> a -> Polygon s a 

instance Functor (Polygon s) where 
    fmap f (Poly pts x) = Poly pts $ f x
 
unitBox3 :: (NDims s ~ Three) => [Polygon s ()]
unitBox3 = uB where
  fz = Poly [pnt orig, pnt uvx, pnt (uvx+uvy), pnt uvy] ()
  fx = Poly [pnt orig, pnt uvy, pnt (uvz+uvy), pnt uvz] ()
  fy = Poly [pnt orig, pnt uvz, pnt (uvz+uvx), pnt uvx] ()
  uB = [fz, fx, fy, translate uvz fz, translate uvx fx, translate uvy fy] 

polyTag :: a -> Polygon s () -> Polygon s a 
polyTag x (Poly p _) =  Poly p x

stretchPoly :: Vec (NDims s) Double -> Polygon s a -> Polygon s a
stretchPoly v (Poly pts a) = Poly (map g pts) a
   where g (Point vp x) = Point (vp*v) x

data Shape s a where
    Polys :: [Polygon s a] -> Shape s a
    Translate :: Vec (NDims s) Double -> Shape s a -> Shape s a

instance Functor (Shape s) where 
    fmap f (Polys polys) = Polys $ map (fmap f) polys
    fmap f (Translate v shp) = Translate v $ fmap f shp


instance Transform Polygon where
    translate vec (Poly pts x) = Poly (translates vec pts) x

instance Transform Shape where
    translate vec shp = Translate vec shp

instance Show a => Show (Polygon s a) where
    show (Poly pts x) = "Poly "++show pts ++ " "++show x

instance HasSpace (Polygon s a) where
    type TheSpace (Polygon s a) = s


