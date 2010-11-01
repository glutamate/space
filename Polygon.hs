{-# OPTIONS_GHC -fglasgow-exts #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Polygon where

import Nats
import VectorsL
import Space
import TNUtils

data Polygon n a where 
     Poly :: [Vec n Double] -> a -> Polygon n a 

data Quads n a where 
     Quads :: [(Vec Four (Vec n Double), Vec n Double)] -> a -> Quads n a 

quadMapVs f (Quads vs x)  = Quads (map (onFst f) vs) x

quadRev = quadMapVs vrev

instance Functor (Polygon n) where 
    fmap f (Poly pts x) = Poly pts $ f x
 
instance Functor (Quads n) where 
    fmap f (Quads pts x) = Quads pts $ f x

unitBox3 :: Quads Three ()
unitBox3 = uB where
  fz,fx, fy :: (Vec Four (Vec Three Double), Vec Three Double)
  fz =  ((orig`vcons`uvx`vcons`(uvx+uvy)`vcons`uvy`vcons`vnil), negate uvz)
  fx =  ((orig`vcons`uvy`vcons`(uvz+uvy)`vcons`uvz`vcons`vnil), negate uvx)
  fy =  ((orig`vcons`uvz`vcons`(uvz+uvx)`vcons`uvx`vcons`vnil), negate uvy)
  otherSide :: (Vec Four (Vec Three Double), Vec Three Double) -> (Vec Four (Vec Three Double), Vec Three Double)
  otherSide (vs, normal) = ((fmap (subtract normal) . vrev) vs, negate normal) 
  shiftBox = Quads [fz, fy,  fx, otherSide fx,
                                 otherSide fy,
                                 otherSide fz] () 
  uB = translate (negate $ uvx/2+uvz/2+uvy/2) $ shiftBox




stretchPoly :: Nat n => Vec n Double -> Polygon n a -> Polygon n a
stretchPoly v (Poly pts a) = Poly (map g pts) a
   where g (vp) = vp*v

{-data Shape n a where
    Polys :: [Polygon n a] -> Shape n a
    Translate :: Vec n Double -> Shape n a -> Shape n a 

instance Functor (Shape s) where 
    fmap f (Polys polys) = Polys $ map (fmap f) polys
    fmap f (Translate v shp) = Translate v $ fmap f shp -}

 
instance Transform Polygon where
    translate vec (Poly pts x) = Poly (map (+vec) pts) x
    stretch = stretchPoly

instance Transform Quads where
    translate vec = quadMapVs $ fmap (+vec)
    stretch v  = quadMapVs $ fmap g
          where g (vp) = vp*v

{-instance Transform Shape where
    translate vec shp = Translate vec shp -}

instance Show a => Show (Polygon n a) where
    show (Poly pts x) = "Poly "++show pts ++ " "++show x



